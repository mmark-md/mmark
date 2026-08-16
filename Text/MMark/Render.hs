{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Text.MMark.Render
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Everything needed to write a render extension, that is, an extension that
-- changes the way an element of a markdown document is turned into HTML.
--
-- A render extension cannot be applied ahead of time, because it needs the
-- rendering function it is wrapping, so renders are collected in a
-- 'RenderExtension' value and given to 'render'. A render cannot fail.
-- Anything that can fail belongs in a transformation, see
-- "Text.MMark.Trans".
--
-- @since 0.0.8.0
module Text.MMark.Render
  ( -- * Rendering
    RenderExtension,
    render,

    -- * Render extension construction
    blockRender,
    inlineRender,
    Ois,
    getOis,

    -- * Documents
    Bni,
    Block (..),
    CellAlign (..),
    Inline (..),
    Span (..),
    blockSpan,
    inlineSpan,

    -- * Rendering machinery
    Render (..),
    applyBlockRender,
    defaultBlockRender,
    applyInlineRender,
    defaultInlineRender,
    newline,

    -- * Utils
    asPlainText,
    headerId,
    headerFragment,
  )
where

import Control.Arrow
import Control.Monad
import Data.Char (isSpace)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Lucid
import Text.MMark.Internal.Type
import Text.MMark.Util
import Text.URI qualified as URI

-- | Render an 'MMark' markdown document. You can then render @'Html' ()@ to
-- various things:
--
--     * to lazy 'Data.Text.Lazy.Text' with 'renderText'
--     * to lazy 'Data.ByteString.Lazy.ByteString' with 'renderBS'
--     * directly to file with 'renderToFile'
--
-- __Note__: the type of this function changed in /0.1.0.0/.
render :: RenderExtension -> MMark -> Html ()
render RenderExtension {..} MMark {..} =
  mapM_ rBlock mmarkBlocks
  where
    rBlock = applyBlockRender extBlockRender . fmap rInlines
    rInlines =
      mkOisInternal &&& mapM_ (applyInlineRender extInlineRender)

-- | Apply a 'Render' to a given @'Block' 'Html' ()@.
--
-- __Note__: the type of this function changed in /0.1.0.0/.
--
-- @since 0.0.8.0
applyBlockRender ::
  Render (Block (Ois, Html ())) ->
  Block (Ois, Html ()) ->
  Html ()
applyBlockRender r = fix (runRender r . defaultBlockRender)

-- | The default 'Block' render.
--
-- __Note__: the type of this function changed in /0.1.0.0/.
--
-- @since 0.0.8.0
defaultBlockRender ::
  -- | Rendering function to use to render sub-blocks
  (Block (Ois, Html ()) -> Html ()) ->
  Block (Ois, Html ()) ->
  Html ()
defaultBlockRender rBlock = \case
  ThematicBreak _ ->
    hr_ [] >> newline
  Heading1 _ (h, html) ->
    h1_ (mkId h) html >> newline
  Heading2 _ (h, html) ->
    h2_ (mkId h) html >> newline
  Heading3 _ (h, html) ->
    h3_ (mkId h) html >> newline
  Heading4 _ (h, html) ->
    h4_ (mkId h) html >> newline
  Heading5 _ (h, html) ->
    h5_ (mkId h) html >> newline
  Heading6 _ (h, html) ->
    h6_ (mkId h) html >> newline
  CodeBlock _ infoString txt -> do
    let f x = class_ $ "language-" <> T.takeWhile (not . isSpace) x
    pre_ $ code_ (maybe [] (pure . f) infoString) (toHtml txt)
    newline
  Naked _ (_, html) ->
    html >> newline
  Paragraph _ (_, html) ->
    p_ html >> newline
  Blockquote _ blocks -> do
    blockquote_ (newline <* mapM_ rBlock blocks)
    newline
  OrderedList _ i items -> do
    let startIndex = [start_ (T.pack $ show i) | i /= 1]
    ol_ startIndex $ do
      newline
      forM_ items $ \x -> do
        li_ (newline <* mapM_ rBlock x)
        newline
    newline
  UnorderedList _ items -> do
    ul_ $ do
      newline
      forM_ items $ \x -> do
        li_ (newline <* mapM_ rBlock x)
        newline
    newline
  Table _ calign (hs :| rows) -> do
    table_ $ do
      newline
      thead_ $ do
        newline
        tr_ $
          forM_ (NE.zip calign hs) $ \(a, h) ->
            th_ (alignStyle a) (snd h)
        newline
      newline
      tbody_ $ do
        newline
        forM_ rows $ \row -> do
          tr_ $
            forM_ (NE.zip calign row) $ \(a, h) ->
              td_ (alignStyle a) (snd h)
          newline
      newline
    newline
  where
    mkId ois = [(id_ . headerId . getOis) ois]
    alignStyle = \case
      CellAlignDefault -> []
      CellAlignLeft -> [style_ "text-align:left"]
      CellAlignRight -> [style_ "text-align:right"]
      CellAlignCenter -> [style_ "text-align:center"]

-- | Apply a render to a given 'Inline'.
--
-- __Note__: the type of this function changed in /0.1.0.0/.
--
-- @since 0.0.8.0
applyInlineRender :: Render Inline -> Inline -> Html ()
applyInlineRender r = fix (runRender r . defaultInlineRender)

-- | The default render for 'Inline' elements.
--
-- __Note__: the type of this function changed in /0.1.0.0/.
--
-- @since 0.0.8.0
defaultInlineRender ::
  -- | Rendering function to use to render sub-inlines
  (Inline -> Html ()) ->
  Inline ->
  Html ()
defaultInlineRender rInline = \case
  Plain _ txt ->
    toHtml txt
  LineBreak _ ->
    br_ [] >> newline
  Emphasis _ inner ->
    em_ (mapM_ rInline inner)
  Strong _ inner ->
    strong_ (mapM_ rInline inner)
  Strikeout _ inner ->
    del_ (mapM_ rInline inner)
  Subscript _ inner ->
    sub_ (mapM_ rInline inner)
  Superscript _ inner ->
    sup_ (mapM_ rInline inner)
  CodeSpan _ txt ->
    code_ (toHtml txt)
  Link _ inner dest mtitle ->
    let title = maybe [] (pure . title_) mtitle
     in a_ (href_ (URI.render dest) : title) (mapM_ rInline inner)
  Image _ desc src mtitle ->
    let title = maybe [] (pure . title_) mtitle
     in img_ (alt_ (asPlainText desc) : src_ (URI.render src) : title)

-- | HTML containing a newline.
--
-- @since 0.0.8.0
newline :: Html ()
newline = "\n"

-- | Create an extension that replaces or augments rendering of 'Block's of
-- a markdown document. The argument of 'blockRender' will be given the
-- rendering function constructed so far @'Block' ('Ois', 'Html' ()) ->
-- 'Html' ()@ as well as an actual block to render—@'Block' ('Ois', 'Html'
-- ())@. The user can then decide whether to replace\/reuse that function to
-- get the final rendering of the type @'Html' ()@.
--
-- See also: 'Ois' and 'getOis'.
--
-- __Note__: the type of this function changed in /0.1.0.0/.
blockRender ::
  ( (Block (Ois, Html ()) -> Html ()) ->
    Block (Ois, Html ()) ->
    Html ()
  ) ->
  RenderExtension
blockRender f = mempty {extBlockRender = Render f}

-- | Create an extension that replaces or augments rendering of 'Inline's of
-- a markdown document. This works like 'blockRender'.
--
-- __Note__: the type of this function changed in /0.1.0.0/.
inlineRender ::
  ((Inline -> Html ()) -> Inline -> Html ()) ->
  RenderExtension
inlineRender f = mempty {extInlineRender = Render f}
