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
  , runScanner
  , useExtension
  , useExtensions
  , render
  , Bni
  , Block (..)
  , Inline (..)
  , Render (..)
  , defaultBlockRender
  , defaultInlineRender
  , asPlainText )
where

import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Char (isSpace)
import Data.Data (Data)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Lucid
import Text.URI (URI)
import qualified Control.Foldl as L
import qualified Data.Text     as T
import qualified Text.URI      as URI

-- | Representation of complete markdown document. You can't look inside of
-- 'MMark' on purpose. The only way to influence an 'MMark' document you
-- obtain as a result of parsing is via the extension mechanism.

data MMark = MMark
  { mmarkYaml :: Maybe Value
    -- ^ Parsed YAML document at the beginning (optional)
  , mmarkBlocks :: [Bni]
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
  { extBlockTrans :: Endo Bni
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

-- | Scan an 'MMark' document efficiently in one pass. This uses the
-- excellent 'L.Fold' type, which see.
--
-- Take a look at the "Text.MMark.Extension" module if you want to create
-- scanners of your own.

runScanner
  :: MMark             -- ^ Document to scan
  -> L.Fold Bni a      -- ^ 'L.Fold' to use
  -> a                 -- ^ Result of scanning
runScanner MMark {..} f = L.fold f mmarkBlocks
{-# INLINE runScanner #-}

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
      . fmap (mapM_ (applyInlineRender extInlineRender) .
              fmap  (appEndo extInlineTrans))
      . appEndo extBlockTrans

-- | A shortcut for the frequently used type @'Block' ('NonEmpty'
-- 'Inline')@.

type Bni = Block (NonEmpty Inline)

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
  | Paragraph a
    -- ^ Paragraph, leaf block
  | Blockquote [Block a]
    -- ^ Blockquote container block
  | OrderedList (NonEmpty [Block a])
    -- ^ Ordered list, container block
  | UnorderedList (NonEmpty [Block a])
    -- ^ Unordered list, container block
  | Naked a
    -- ^ Naked content, without an enclosing tag
  deriving (Show, Eq, Ord, Data, Typeable, Generic, Functor, Foldable)

instance NFData a => NFData (Block a)

-- | Inline markdown content.

data Inline
  = Plain Text
    -- ^ Plain text
  | LineBreak
    -- ^ Line break (hard)
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
  | Link (NonEmpty Inline) URI (Maybe Text)
    -- ^ Link with text, destination, and optionally title
  | Image (NonEmpty Inline) URI (Maybe Text)
    -- ^ Image with description, URL, and optionally title
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
  Paragraph html ->
    p_ html >> newline
  Blockquote blocks ->
    blockquote_ (mapM_ defaultBlockRender blocks)
  OrderedList items -> do
    ol_ $ do
      newline
      forM_ items $ \x -> do
        li_ (mapM_ defaultBlockRender x)
        newline
    newline
  UnorderedList items -> do
    ul_ $ do
      newline
      forM_ items $ \x -> do
        li_ (mapM_ defaultBlockRender x)
        newline
    newline
  Naked html ->
    html

-- | Apply a render to a given 'Inline'.

applyInlineRender :: Render Inline -> Inline -> Html ()
applyInlineRender r = getRender r defaultInlineRender

-- | The default render for 'Inline' elements. Comments about
-- 'defaultBlockRender' apply here just as well.

defaultInlineRender :: Inline -> Html ()
defaultInlineRender = \case
  Plain txt ->
    toHtml txt
  LineBreak ->
    br_ [] >> newline
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
    in a_ (href_ (URI.render dest) : title) (mapM_ defaultInlineRender inner)
  Image desc src mtitle ->
    let title = maybe [] (pure . title_) mtitle
    in img_ (alt_ (asPlainText desc) : src_ (URI.render src) : title)

-- | HTML containing a newline.

newline :: Html ()
newline = "\n"

----------------------------------------------------------------------------
-- Utils

-- | Convert a non-empty collection of 'Inline's into their plain text
-- representation. This is used e.g. to render image descriptions.

asPlainText :: NonEmpty Inline -> Text
asPlainText = foldMap $ \case
  Plain      txt -> txt
  LineBreak      -> "\n"
  Emphasis    xs -> asPlainText xs
  Strong      xs -> asPlainText xs
  Strikeout   xs -> asPlainText xs
  Subscript   xs -> asPlainText xs
  Superscript xs -> asPlainText xs
  CodeSpan   txt -> txt
  Link    xs _ _ -> asPlainText xs
  Image   xs _ _ -> asPlainText xs
