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
-- MMark rendering machinery.
--
-- @since 0.0.8.0
module Text.MMark.Render
  ( render,
    applyBlockRender,
    defaultBlockRender,
    applyInlineRender,
    defaultInlineRender,
    newline,
  )
where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Data.Char (isSpace)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Lucid
import Text.MMark.Internal.Type
import Text.MMark.Trans
import Text.MMark.Util
import Text.URI qualified as URI

-- | Render a 'MMark' markdown document. You can then render @'Html' ()@ to
-- various things:
--
--     * to lazy 'Data.Taxt.Lazy.Text' with 'renderText'
--     * to lazy 'Data.ByteString.Lazy.ByteString' with 'renderBS'
--     * directly to file with 'renderToFile'
renderM :: forall m. (Monad m) => MMarkT m -> HtmlT m ()
renderM MMark {..} =
  mapM_ rBlock mmarkBlocks
  where
    Extension {..} = mmarkExtension

    rBlock :: Bni -> HtmlT m ()
    rBlock x0 = do
      x1 <- lift $ applyBlockTrans extBlockTrans x0
      x2 <- lift $ traverse rInlines x1
      applyBlockRender extBlockRender x2

    rInlines :: NonEmpty Inline -> m (Ois, HtmlT m ())
    rInlines x0 = do
      x1 <- traverse (applyInlineTrans extInlineTrans) x0
      pure $ (mkOisInternal &&& mapM_ (applyInlineRender extInlineRender)) x1

-- | 'renderM' specialized to `Identity`.
render :: MMark -> Html ()
render = renderM

-- | Apply a 'Render' to a given @'Block' 'Html' ()@.
--
-- @since 0.0.8.0
applyBlockRender ::
  (Monad m) =>
  RenderT m (Block (Ois, HtmlT m ())) ->
  Block (Ois, HtmlT m ()) ->
  HtmlT m ()
applyBlockRender r = fix (appEndo r . defaultBlockRender)

-- | The default 'Block' render.
--
-- @since 0.0.8.0
defaultBlockRender ::
  (Monad m) =>
  -- | Rendering function to use to render sub-blocks
  (Block (Ois, HtmlT m ()) -> HtmlT m ()) ->
  Block (Ois, HtmlT m ()) ->
  HtmlT m ()
defaultBlockRender blockRender = \case
  ThematicBreak ->
    hr_ [] >> newline
  Heading1 (h, html) ->
    h1_ (mkId h) html >> newline
  Heading2 (h, html) ->
    h2_ (mkId h) html >> newline
  Heading3 (h, html) ->
    h3_ (mkId h) html >> newline
  Heading4 (h, html) ->
    h4_ (mkId h) html >> newline
  Heading5 (h, html) ->
    h5_ (mkId h) html >> newline
  Heading6 (h, html) ->
    h6_ (mkId h) html >> newline
  CodeBlock infoString txt -> do
    let f x = class_ $ "language-" <> T.takeWhile (not . isSpace) x
    pre_ $ code_ (maybe [] (pure . f) infoString) (toHtml txt)
    newline
  Naked (_, html) ->
    html >> newline
  Paragraph (_, html) ->
    p_ html >> newline
  Blockquote blocks -> do
    blockquote_ (newline <* mapM_ blockRender blocks)
    newline
  OrderedList i items -> do
    let startIndex = [start_ (T.pack $ show i) | i /= 1]
    ol_ startIndex $ do
      newline
      forM_ items $ \x -> do
        li_ (newline <* mapM_ blockRender x)
        newline
    newline
  UnorderedList items -> do
    ul_ $ do
      newline
      forM_ items $ \x -> do
        li_ (newline <* mapM_ blockRender x)
        newline
    newline
  Table calign (hs :| rows) -> do
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
-- @since 0.0.8.0
applyInlineRender :: (Monad m) => RenderT m Inline -> Inline -> HtmlT m ()
applyInlineRender r = fix (appEndo r . defaultInlineRender)

-- | The default render for 'Inline' elements.
--
-- @since 0.0.8.0
defaultInlineRender ::
  (Monad m) =>
  -- | Rendering function to use to render sub-inlines
  (Inline -> HtmlT m ()) ->
  Inline ->
  HtmlT m ()
defaultInlineRender inlineRender = \case
  Plain txt ->
    toHtml txt
  LineBreak ->
    br_ [] >> newline
  Emphasis inner ->
    em_ (mapM_ inlineRender inner)
  Strong inner ->
    strong_ (mapM_ inlineRender inner)
  Strikeout inner ->
    del_ (mapM_ inlineRender inner)
  Subscript inner ->
    sub_ (mapM_ inlineRender inner)
  Superscript inner ->
    sup_ (mapM_ inlineRender inner)
  CodeSpan txt ->
    code_ (toHtml txt)
  Link inner dest mtitle ->
    let title = maybe [] (pure . title_) mtitle
     in a_ (href_ (URI.render dest) : title) (mapM_ inlineRender inner)
  Image desc src mtitle ->
    let title = maybe [] (pure . title_) mtitle
     in img_ (alt_ (asPlainText desc) : src_ (URI.render src) : title)

-- | HTML containing a newline.
--
-- @since 0.0.8.0
newline :: (Monad m) => HtmlT m ()
newline = "\n"
