{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Text.MMark.Type
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal type definitions. Some of these are re-exported in the public
-- modules.
module Text.MMark.Type
  ( MMark,
    MMarkM (..),
    Extension,
    ExtensionM (..),
    Render,
    runReader,
    RenderT (..),
    Bni,
    Block (..),
    CellAlign (..),
    Inline (..),
    Ois,
    mkOisInternal,
    getOis,
  )
where

import Control.Arrow
import qualified Control.Category as Category
import Control.DeepSeq
import Data.Aeson
import Data.Data (Data)
import Data.Function (on)
import Data.Functor.Identity
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Lucid
import Text.URI (URI (..))

-- | Representation of complete markdown document. You can't look inside of
-- 'MMark' on purpose. The only way to influence an 'MMark' document you
-- obtain as a result of parsing is via the extension mechanism.
type MMark = MMarkM Identity

data MMarkM m = MMarkM
  { -- | Parsed YAML document at the beginning (optional)
    mmarkYaml :: Maybe Value,
    -- | Actual contents of the document
    mmarkBlocks :: [Bni],
    -- | Extension specifying how to process and render the blocks
    mmarkExtension :: ExtensionM m
  }

instance NFData (MMarkM m) where
  rnf MMarkM {..} = rnf mmarkYaml `seq` rnf mmarkBlocks

-- | Dummy instance.
--
-- @since 0.0.5.0
instance Show (MMarkM m) where
  show = const "MMark {..}"

-- | An extension. You can apply extensions with 'Text.MMark.useExtension'
-- and 'Text.MMark.useExtensions' functions. The "Text.MMark.Extension"
-- module provides tools for writing your own extensions.
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
type Extension = ExtensionM Identity

data ExtensionM (m :: Type -> Type) = ExtensionM
  { -- | Block transformation
    extBlockTrans :: Kleisli m Bni Bni,
    -- | Block render
    extBlockRender :: RenderT m (Block (Ois, HtmlT m ())),
    -- | Inline transformation
    extInlineTrans :: Kleisli m Inline Inline,
    -- | Inline render
    extInlineRender :: RenderT m Inline
  }

instance Monad m => Semigroup (ExtensionM m) where
  x <> y =
    ExtensionM
      { extBlockTrans = ((<<<) `on` extBlockTrans) x y,
        extBlockRender = ((<>) `on` extBlockRender) x y,
        extInlineTrans = ((<<<) `on` extInlineTrans) x y,
        extInlineRender = ((<>) `on` extInlineRender) x y
      }

instance Monad m => Monoid (ExtensionM m) where
  mempty =
    ExtensionM
      { extBlockTrans = Category.id,
        extBlockRender = mempty,
        extInlineTrans = Category.id,
        extInlineRender = mempty
      }
  mappend = (<>)

-- | An internal type that captures the extensible rendering process we use.
-- 'Render' has a function inside which transforms a rendering function of
-- the type @a -> Html ()@.
type Render a = RenderT Identity a

runReader :: Render a -> (a -> Html ()) -> a -> Html ()
runReader = runRenderT

newtype RenderT m a = RenderT
  {runRenderT :: (a -> HtmlT m ()) -> a -> HtmlT m ()}

instance Semigroup (RenderT m a) where
  RenderT f <> RenderT g = RenderT (f . g)

instance Monoid (RenderT m a) where
  mempty = RenderT id
  mappend = (<>)

-- | A shortcut for the frequently used type @'Block' ('NonEmpty'
-- 'Inline')@.
type Bni = Block (NonEmpty Inline)

-- | We can think of a markdown document as a collection of
-- blocks—structural elements like paragraphs, block quotations, lists,
-- headings, thematic breaks, and code blocks. Some blocks (like block
-- quotes and list items) contain other blocks; others (like headings and
-- paragraphs) contain inline content, see 'Inline'.
--
-- We can divide blocks into two types: container blocks, which can contain
-- other blocks, and leaf blocks, which cannot.
data Block a
  = -- | Thematic break, leaf block
    ThematicBreak
  | -- | Heading (level 1), leaf block
    Heading1 a
  | -- | Heading (level 2), leaf block
    Heading2 a
  | -- | Heading (level 3), leaf block
    Heading3 a
  | -- | Heading (level 4), leaf block
    Heading4 a
  | -- | Heading (level 5), leaf block
    Heading5 a
  | -- | Heading (level 6), leaf block
    Heading6 a
  | -- | Code block, leaf block with info string and contents
    CodeBlock (Maybe Text) Text
  | -- | Naked content, without an enclosing tag
    Naked a
  | -- | Paragraph, leaf block
    Paragraph a
  | -- | Blockquote container block
    Blockquote [Block a]
  | -- | Ordered list ('Word' is the start index), container block
    OrderedList Word (NonEmpty [Block a])
  | -- | Unordered list, container block
    UnorderedList (NonEmpty [Block a])
  | -- | Table, first argument is the alignment options, then we have a
    -- 'NonEmpty' list of rows, where every row is a 'NonEmpty' list of
    -- cells, where every cell is an @a@ thing.
    --
    -- The first row is always the header row, because pipe-tables that we
    -- support cannot lack a header row.
    --
    -- @since 0.0.4.0
    Table (NonEmpty CellAlign) (NonEmpty (NonEmpty a))
  deriving (Show, Eq, Ord, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (Block a)

-- | Options for cell alignment in tables.
--
-- @since 0.0.4.0
data CellAlign
  = -- | No specific alignment specified
    CellAlignDefault
  | -- | Left-alignment
    CellAlignLeft
  | -- | Right-alignment
    CellAlignRight
  | -- | Center-alignment
    CellAlignCenter
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance NFData CellAlign

-- | Inline markdown content.
data Inline
  = -- | Plain text
    Plain Text
  | -- | Line break (hard)
    LineBreak
  | -- | Emphasis
    Emphasis (NonEmpty Inline)
  | -- | Strong emphasis
    Strong (NonEmpty Inline)
  | -- | Strikeout
    Strikeout (NonEmpty Inline)
  | -- | Subscript
    Subscript (NonEmpty Inline)
  | -- | Superscript
    Superscript (NonEmpty Inline)
  | -- | Code span
    CodeSpan Text
  | -- | Link with text, destination, and optionally title
    Link (NonEmpty Inline) URI (Maybe Text)
  | -- | Image with description, URL, and optionally title
    Image (NonEmpty Inline) URI (Maybe Text)
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance NFData Inline

-- | A wrapper for “original inlines”. Source inlines are wrapped in this
-- during rendering of inline components and then it's available to block
-- render, but only for inspection. Altering of 'Ois' is not possible
-- because the user cannot construct a value of the 'Ois' type, he\/she can
-- only inspect it with 'getOis'.
newtype Ois = Ois (NonEmpty Inline)

-- | Make an 'Ois' value. This is an internal constructor that should not be
-- exposed!
mkOisInternal :: NonEmpty Inline -> Ois
mkOisInternal = Ois

-- | Project @'NonEmpty' 'Inline'@ from 'Ois'.
getOis :: Ois -> NonEmpty Inline
getOis (Ois inlines) = inlines
