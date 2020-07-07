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
module Text.MMark.Render
  ( render,
  )
where

import Control.Arrow
import Control.Monad
import Data.Char (isSpace)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Lucid
import Text.MMark.Trans
import Text.MMark.Type
import Text.MMark.Util
import qualified Text.URI as URI

-- | Render a 'MMark' markdown document. You can then render @'Html' ()@ to
-- various things:
--
--     * to lazy 'Data.Taxt.Lazy.Text' with 'renderText'
--     * to lazy 'Data.ByteString.Lazy.ByteString' with 'renderBS'
--     * directly to file with 'renderToFile'
render :: MMark -> Html ()
render MMark {..} =
  mapM_ rBlock mmarkBlocks
  where
    Extension {..} = mmarkExtension
    rBlock =
      applyBlockRender extBlockRender
        . fmap rInlines
        . applyBlockTrans extBlockTrans
    rInlines =
      (mkOisInternal &&& mapM_ (applyInlineRender extInlineRender))
        . fmap (applyInlineTrans extInlineTrans)

-- | Apply a 'Render' to a given @'Block' 'Html' ()@.
applyBlockRender ::
  Render (Block (Ois, Html ())) ->
  Block (Ois, Html ()) ->
  Html ()
applyBlockRender r = fix (runRender r . defaultBlockRender)

-- | The default 'Block' render.
defaultBlockRender ::
  -- | Rendering function to use to render sub-blocks
  (Block (Ois, Html ()) -> Html ()) ->
  Block (Ois, Html ()) ->
  Html ()
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
applyInlineRender :: Render Inline -> Inline -> Html ()
applyInlineRender r = fix (runRender r . defaultInlineRender)

-- | The default render for 'Inline' elements.
defaultInlineRender ::
  -- | Rendering function to use to render sub-inlines
  (Inline -> Html ()) ->
  Inline ->
  Html ()
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
newline :: Html ()
newline = "\n"
