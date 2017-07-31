-- |
-- Module      :  Text.MMark.Internal
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal definitions you really shouldn't import. Import "Text.MMark"
-- instead.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}

module Text.MMark.Internal
  ( MMark (..)
  , Extension (..)
  , useExtension
  , useExtensions
  , renderMMark
  , Block (..)
  , Inline (..)
  , defaultBlockRender
  , defaultInlineRender )
where

import Control.Monad
import Data.Aeson
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Lucid
import qualified Data.Text.Lazy as TL

-- | Representation of complete markdown document.

data MMark = MMark
  { mmarkYaml_       :: Maybe Value
    -- ^ Parsed YAML document at the beginning (optional)
  , mmarkBlocks      :: [Block Inline]
    -- ^
  , mmarkBlockTrans  :: Endo (Block Inline)
  , mmarkBlockRender :: Render (Block Inline)
  , mmarkInlineTrans :: Endo Inline
  , mmarkInlineRender :: Render Inline
  }

-- | TODO We should allow the process optionally fail perhaps?

newtype Render a = Render (a -> Html () -> Html ())

instance Semigroup (Render a) where
  Render f <> Render g = Render $ \elt html ->
    g elt (f elt html)

instance Monoid (Render a) where
  mempty  = Render (const id)
  mappend = (<>)

-- TODO this stuff should be moved from here

data Extension -- FIXME not quite right for being a monoid
  = TransformBlock (Endo (Block Inline))
  | RenderBlock (Render (Block Inline))
  | TransformInline (Endo Inline)
  | RenderInline (Render Inline)

instance Semigroup Extension where
  (<>) = undefined -- TODO

instance Monoid Extension where
  mempty = undefined -- TODO
  mappend = (<>)

useExtension :: Extension -> MMark -> MMark
useExtension ext mmark =
  case ext of
    TransformBlock f ->
      mmark { mmarkBlockTrans = mmarkBlockTrans mmark <> f }
    RenderBlock f ->
      mmark { mmarkBlockRender = mmarkBlockRender mmark <> f }
    TransformInline f ->
      mmark { mmarkInlineTrans = mmarkInlineTrans mmark <> f }
    RenderInline f ->
      mmark { mmarkInlineRender = mmarkInlineRender mmark <> f }

useExtensions :: [Extension] -> MMark -> MMark
useExtensions exts = useExtension (mconcat exts)

-- | Render a 'MMark' markdown document as a lazy 'TL.Text' value.

renderMMark :: MMark -> TL.Text
renderMMark = undefined -- TODO

-- | We can think of a markdown document as a collection of
-- blocks—structural elements like paragraphs, block quotations, lists,
-- headings, rules, and code blocks. Some blocks (like block quotes and list
-- items) contain other blocks; others (like headings and paragraphs)
-- contain inline content, see 'Inline'.
--
-- We can divide blocks into two types: container blocks, which can contain
-- other blocks, and leaf blocks, which cannot.

data Block a
  = ThematicBreak
    -- ^ Thematic break, leaf block
  | Heading1 a
    -- ^ Heading (level 1), leaf block
  | Heading2 a
    -- ^ Heading (level 2), leaf block
  | Heading3 a
    -- ^ Heading (level 3), leaf block
  | Heading4 a
    -- ^ Heading (level 4), leaf block
  | Heading5 a
    -- ^ Heading (level 5), leaf block
  | Heading6 a
    -- ^ Heading (level 6), leaf block
  | CodeBlock (Maybe Text) Text
    -- ^ Code block, leaf block with info string and contents
  | HtmlBlock Text
    -- ^ HTML block, leaf block
  | Paragraph a
    -- ^ Paragraph, leaf block
  | Blockquote [Block a]
    -- ^ Blockquote container block
  | OrderedList (NonEmpty (Block a))
    -- ^ Ordered list, container block
  | UnorderedList (NonEmpty (Block a))
    -- ^ Unordered list, container block
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- TODO LinkReferenceDefiniton Text Text (Maybe Text)
--   -- ^ Line reference definition with name, destination, and optionally title

-- | Inline markdown content.

data Inline
  = Plain Text
    -- ^ Plain text
  | CodeSpan Text
    -- ^ Code span
  | Link Text Text (Maybe Text)
    -- ^ Link with text, destination, and optionally title
  | Image Text Text (Maybe Text)
    -- ^ Image with description, URL, and optionally title
  | HtmlInline Text
    -- ^ Inline HTML
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- TODO ReferenceLink Text Text
--    -- ^ Reference link with text and label

-- NOTE We must simplify all links as part of parsing so we have normalized
-- links later.

----------------------------------------------------------------------------
-- Default renders

defaultBlockRender :: Render (Block (Html ()))
defaultBlockRender = Render $ \block _ ->
  case block of
    ThematicBreak ->
      br_ []
    Heading1 html ->
      h1_ html
    Heading2 html ->
      h2_ html
    Heading3 html ->
      h3_ html
    Heading4 html ->
      h4_ html
    Heading5 html ->
      h5_ html
    Heading6 html ->
      h6_ html
    CodeBlock _ txt ->
      (pre_ . code_ . toHtmlRaw) txt
    HtmlBlock txt ->
      toHtmlRaw txt
    Paragraph html ->
      p_ html
    Blockquote blocks ->
      blockquote_ (mapM_ renderSubBlock blocks)
    OrderedList items ->
      ol_ $ forM_ items (li_ . renderSubBlock)
    UnorderedList items ->
      ul_ $ forM_ items (li_ . renderSubBlock)
  where
    renderSubBlock x =
      let (Render f) = defaultBlockRender in f x (return ())

defaultInlineRender :: Render Inline
defaultInlineRender = Render $ \inline _ ->
  case inline of
    Plain txt ->
      toHtml txt
    CodeSpan txt ->
      code_ (toHtmlRaw txt)
    Link txt dest mtitle ->
      let title = maybe [] (pure . title_) mtitle
      in a_ (href_ dest : title) (toHtml txt)
    Image alt src mtitle ->
      let title = maybe [] (pure . title_) mtitle
      in img_ (alt_ alt : src_ src : title)
    HtmlInline txt ->
      toHtmlRaw txt
