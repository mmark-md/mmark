-- |
-- Module      :  Text.MMark.Internal
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal definitions.

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Text.MMark.Internal
  ( MMark (..)
  , Extension (..)
  , Scanner (..)
  , runScanner
  , (.&+)
  , useExtension
  , useExtensions
  , render
  , Block (..)
  , Inline (..)
  , Render (..)
  , defaultBlockRender
  , defaultInlineRender )
where

import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Char (isSpace)
import Data.Data (Data)
import Data.Function (on)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Lucid
import qualified Data.Text as T

-- | Representation of complete markdown document. You can't look inside of
-- 'MMark' on purpose. The only way to influence an 'MMark' document you
-- obtain as a result of parsing is via the extension mechanism.

data MMark = MMark
  { mmarkYaml :: Maybe Value
    -- ^ Parsed YAML document at the beginning (optional)
  , mmarkBlocks :: [Block (NonEmpty Inline)]
    -- ^ Actual contents of the document
  , mmarkExtension :: Extension
    -- ^ Extension specifying how to process and render the blocks
  }

-- | An extension. You can apply extensions with 'useExtension' and
-- 'useExtensions' functions. The "Text.MMark.Extension" module provides
-- tools for extension creation.
--
-- Note that 'Extension' is an instance of 'Semigroup' and 'Monoid', i.e.
-- you can combine several extensions into one. Since the @('<>')@ operator
-- is right-associative and 'mconcat' is a right fold under the hood, the
-- expression
--
-- > l <> r
--
-- means that the extension @r@ will be applied before the extension @l@,
-- similar to how 'Endo' works. This may seem counter-intuitive, but only
-- with this logic we get consistency of ordering with more complex
-- expressions:
--
-- > e2 <> e1 <> e0 == e2 <> (e1 <> e0)
--
-- Here, @e0@ will be applied first, then @e1@, then @e2@. The same applies
-- to expressions involving 'mconcat'—extensions closer to beginning of the
-- list passed to 'mconcat' will be applied later.

data Extension = Extension
  { extBlockTrans :: forall a. Endo (Block a)
    -- ^ Block transformation
  , extBlockRender :: Render (Block (Html ()))
    -- ^ Block render
  , extInlineTrans :: Endo Inline
    -- ^ Inline transformation
  , extInlineRender :: Render Inline
    -- ^ Inline render
  }

instance Semigroup Extension where
  x <> y = Extension
    { extBlockTrans   = on (<>) extBlockTrans   x y
    , extBlockRender  = on (<>) extBlockRender  x y
    , extInlineTrans  = on (<>) extInlineTrans  x y
    , extInlineRender = on (<>) extInlineRender x y }

instance Monoid Extension where
  mempty = Extension
    { extBlockTrans   = mempty
    , extBlockRender  = mempty
    , extInlineTrans  = mempty
    , extInlineRender = mempty }
  mappend = (<>)

-- | Apply an 'Extension' to an 'MMark' document. The order in which you
-- apply 'Extension's /does matter/. Extensions you apply first take effect
-- first. The extension system is designed in such a way that in many cases
-- the order doesn't matter, but sometimes the difference is important.

useExtension :: Extension -> MMark -> MMark
useExtension ext mmark =
  mmark { mmarkExtension = ext <> mmarkExtension mmark }

-- | Apply several 'Extension's to an 'MMark' document.
--
-- This is a simple shortcut:
--
-- > useExtensions exts = useExtension (mconcat exts)
--
-- As mentioned in the docs for 'useExtension', the order in which you apply
-- extensions matters. Extensions closer to beginning of the list are
-- applied later, i.e. the last extension in the list is applied first.

useExtensions :: [Extension] -> MMark -> MMark
useExtensions exts = useExtension (mconcat exts)

-- | A scanner. 'Scanner' is something that can extract information from an
-- 'MMark' document. You can compose 'Scanner's using the @('.&+')@
-- operator. The "Text.MMark.Extension" module provides tools for creation
-- of scanners.

data Scanner a = Scanner (a -> Block (NonEmpty Inline) -> a)

-- | Run a 'Scanner' on an 'MMark'. It's desirable to run it only once
-- because running a scanner is typically an expensive traversal of the
-- whole document. Combine all scanners you need to run into one using the
-- @('.&+')@ operator, then run that.

runScanner
  :: MMark             -- ^ Document to scan
  -> Scanner a         -- ^ Scanner to use
  -> a                 -- ^ Starting value
  -> a                 -- ^ Result of scanning
runScanner MMark {..} (Scanner f) x = foldl' f x mmarkBlocks

-- | Combine two 'Scanner's into one composite scanner.

infixl 2 .&+

(.&+) :: Scanner a -> Scanner b -> Scanner (a, b)
Scanner f .&+ Scanner g = Scanner $ \(!a, !b) block ->
  (f a block, g b block)

-- | Render a 'MMark' markdown document. You can then render @'Html' ()@ to
-- various things:
--
--     * to lazy 'Data.Taxt.Lazy.Text' with 'renderText'
--     * to lazy 'Data.ByteString.Lazy.ByteString' with 'renderBS'
--     * directly to file with 'renderToFile'

render :: MMark -> Html ()
render MMark {..} =
  mapM_ produceBlock mmarkBlocks
  where
    Extension {..} = mmarkExtension
    produceBlock   = applyBlockRender extBlockRender
      . appEndo extBlockTrans
      . fmap (mapM_ (applyInlineRender extInlineRender) .
              fmap  (appEndo extInlineTrans))

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
  deriving (Show, Eq, Ord, Data, Typeable, Generic, Functor, Foldable)

instance NFData a => NFData (Block a)

-- | Inline markdown content.

data Inline
  = Plain Text
    -- ^ Plain text
  | Emphasis (NonEmpty Inline)
    -- ^ Emphasis
  | Strong (NonEmpty Inline)
    -- ^ Strong emphasis
  | Strikeout (NonEmpty Inline)
    -- ^ Strikeout
  | Subscript (NonEmpty Inline)
    -- ^ Subscript
  | Superscript (NonEmpty Inline)
    -- ^ Superscript
  | CodeSpan Text
    -- ^ Code span
  | Link (NonEmpty Inline) Text (Maybe Text)
    -- ^ Link with text, destination, and optionally title
  | Image Text Text (Maybe Text)
    -- ^ Image with description, URL, and optionally title
  | HtmlInline Text
    -- ^ Inline HTML
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance NFData Inline

----------------------------------------------------------------------------
-- Renders

-- | An internal type that captures the extensible rendering process we use.
-- 'Render' has a function inside which transforms a rendering function of
-- the type @a -> Html ()@.

newtype Render a = Render
  { getRender :: (a -> Html ()) -> a -> Html () }

instance Semigroup (Render a) where
  Render f <> Render g = Render $ \h -> f (g h)

instance Monoid (Render a) where
  mempty  = Render id
  mappend = (<>)

-- | Apply a 'Render' to a given @'Block' 'Html' ()@.

applyBlockRender :: Render (Block (Html ())) -> Block (Html ()) -> Html ()
applyBlockRender r = getRender r defaultBlockRender

-- | The default 'Block' render. Note that it does not care about what we
-- have rendered so far because it always starts rendering. Thus it's OK to
-- just pass it something dummy as the second argument of the inner
-- function.

defaultBlockRender :: Block (Html ()) -> Html ()
defaultBlockRender = \case
  ThematicBreak ->
    hr_ [] >> newline
  Heading1 html ->
    h1_ html >> newline
  Heading2 html ->
    h2_ html >> newline
  Heading3 html ->
    h3_ html >> newline
  Heading4 html ->
    h4_ html >> newline
  Heading5 html ->
    h5_ html >> newline
  Heading6 html ->
    h6_ html >> newline
  CodeBlock infoString txt -> do
    let f x = class_ $ "language-" <> T.takeWhile (not . isSpace) x
    pre_ $ code_ (maybe [] (pure . f) infoString) (toHtml txt)
    newline
  HtmlBlock txt ->
    toHtmlRaw txt
  Paragraph html ->
    p_ html >> newline
  Blockquote blocks ->
    blockquote_ (mapM_ defaultBlockRender blocks)
  OrderedList items ->
    ol_ $ forM_ items (li_ . defaultBlockRender)
  UnorderedList items ->
    ul_ $ forM_ items (li_ . defaultBlockRender)
  where
    newline = "\n"

-- | Apply a render to a given 'Inline'.

applyInlineRender :: Render Inline -> Inline -> Html ()
applyInlineRender r = getRender r defaultInlineRender

-- | The default render for 'Inline' elements. Comments about
-- 'defaultBlockRender' apply here just as well.

defaultInlineRender :: Inline -> Html ()
defaultInlineRender = \case
  Plain txt ->
    toHtml txt
  Emphasis inner ->
    em_ (mapM_ defaultInlineRender inner)
  Strong inner ->
    strong_ (mapM_ defaultInlineRender inner)
  Strikeout inner ->
    del_ (mapM_ defaultInlineRender inner)
  Subscript inner ->
    sub_ (mapM_ defaultInlineRender inner)
  Superscript inner ->
    sup_ (mapM_ defaultInlineRender inner)
  CodeSpan txt ->
    code_ (toHtmlRaw txt)
  Link inner dest mtitle ->
    let title = maybe [] (pure . title_) mtitle
    in a_ (href_ dest : title) (mapM_ defaultInlineRender inner)
  Image alt src mtitle ->
    let title = maybe [] (pure . title_) mtitle
    in img_ (alt_ alt : src_ src : title)
  HtmlInline txt ->
    toHtmlRaw txt
