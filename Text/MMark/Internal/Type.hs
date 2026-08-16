{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  Text.MMark.Internal.Type
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal type definitions. The public subset of these is re-exported from
-- "Text.MMark.Trans" and "Text.MMark.Render".
--
-- @since 0.0.8.0
module Text.MMark.Internal.Type
  ( -- * Documents
    MMark (..),
    Bni,
    Block (..),
    CellAlign (..),
    Inline (..),
    Span (..),
    spanUnion,
    blockSpan,
    setBlockSpan,
    inlineSpan,
    setInlineSpan,

    -- * The transformation monad
    TransT,
    Trans,
    runTransT,
    report,
    abort,
    TransError (..),

    -- * Rendering
    RenderExtension (..),
    Render (..),
    Ois,
    mkOisInternal,
    getOis,
  )
where

import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Aeson
import Data.Data (Data)
import Data.Functor.Identity (Identity)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as E
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Lucid
import Text.Megaparsec
import Text.URI (URI (..))

----------------------------------------------------------------------------
-- Documents

-- | Representation of a complete markdown document. You can't look inside
-- of 'MMark' on purpose. The only way to influence an 'MMark' document you
-- obtain as a result of parsing is via the extension mechanism.
data MMark = MMark
  { -- | Parsed YAML document at the beginning (optional)
    mmarkYaml :: Maybe Value,
    -- | Actual contents of the document
    mmarkBlocks :: [Bni],
    -- | The state that allows us to turn the offsets in 'Span's back into
    -- lines and columns when an extension reports an error
    mmarkSource :: PosState Text
  }

instance NFData MMark where
  rnf MMark {..} = rnf mmarkYaml `seq` rnf mmarkBlocks

-- | Dummy instance.
--
-- @since 0.0.5.0
instance Show MMark where
  show = const "MMark {..}"

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
--
-- Every constructor carries the 'Span' of the source it derives from as its
-- first argument, see 'blockSpan'.
--
-- __Note__: the constructors of this type changed in the version /0.1.0.0/.
data Block a
  = -- | Thematic break, leaf block
    ThematicBreak Span
  | -- | Heading (level 1), leaf block
    Heading1 Span a
  | -- | Heading (level 2), leaf block
    Heading2 Span a
  | -- | Heading (level 3), leaf block
    Heading3 Span a
  | -- | Heading (level 4), leaf block
    Heading4 Span a
  | -- | Heading (level 5), leaf block
    Heading5 Span a
  | -- | Heading (level 6), leaf block
    Heading6 Span a
  | -- | Code block, leaf block with info string and contents
    CodeBlock Span (Maybe Text) Text
  | -- | Naked content, without an enclosing tag
    Naked Span a
  | -- | Paragraph, leaf block
    Paragraph Span a
  | -- | Blockquote container block
    Blockquote Span [Block a]
  | -- | Ordered list ('Word' is the start index), container block
    OrderedList Span Word (NonEmpty [Block a])
  | -- | Unordered list, container block
    UnorderedList Span (NonEmpty [Block a])
  | -- | Table, first argument is the alignment options, then we have a
    -- 'NonEmpty' list of rows, where every row is a 'NonEmpty' list of
    -- cells, where every cell is an @a@ thing.
    --
    -- The first row is always the header row, because pipe-tables that we
    -- support cannot lack a header row.
    --
    -- @since 0.0.4.0
    Table Span (NonEmpty CellAlign) (NonEmpty (NonEmpty a))
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

instance (NFData a) => NFData (Block a)

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
  deriving (Show, Eq, Ord, Data, Generic)

instance NFData CellAlign

-- | Inline markdown content.
--
-- Every constructor carries the 'Span' of the source it derives from as its
-- first argument, see 'inlineSpan'.
--
-- __Note__: the constructors of this type changed in the version /0.1.0.0/.
data Inline
  = -- | Plain text
    Plain Span Text
  | -- | Line break (hard)
    LineBreak Span
  | -- | Emphasis
    Emphasis Span (NonEmpty Inline)
  | -- | Strong emphasis
    Strong Span (NonEmpty Inline)
  | -- | Strikeout
    Strikeout Span (NonEmpty Inline)
  | -- | Subscript
    Subscript Span (NonEmpty Inline)
  | -- | Superscript
    Superscript Span (NonEmpty Inline)
  | -- | Code span
    CodeSpan Span Text
  | -- | Link with text, destination, and optionally title
    Link Span (NonEmpty Inline) URI (Maybe Text)
  | -- | Image with description, URL, and optionally title
    Image Span (NonEmpty Inline) URI (Maybe Text)
  deriving (Show, Eq, Ord, Data, Generic)

instance NFData Inline

-- | A region of the source document.
--
-- A 'Span' is the region of the source that a node __derives from__, not
-- necessarily the region it was parsed from. A node that an extension
-- creates in place of another one inherits its 'Span', and a node that an
-- extension assembles from several others should be given the 'spanUnion'
-- of their spans. This way every node in a transformed document can still
-- say which part of the input it came from, which is what makes it possible
-- to report extension errors against the source.
--
-- @since 0.1.0.0
data Span = Span
  { -- | Offset of the first character of the region
    spanStart :: !Int,
    -- | Offset just past the last character of the region
    spanEnd :: !Int
  }
  deriving (Show, Eq, Ord, Data, Generic)

instance NFData Span

-- | The smallest 'Span' that covers both of its arguments.
--
-- @since 0.1.0.0
spanUnion :: Span -> Span -> Span
spanUnion (Span a b) (Span c d) = Span (min a c) (max b d)

-- | @since 0.1.0.0
instance Semigroup Span where
  (<>) = spanUnion

-- | Project the annotation of a 'Block'.
--
-- @since 0.1.0.0
blockSpan :: Block a -> Span
blockSpan = \case
  ThematicBreak spn -> spn
  Heading1 spn _ -> spn
  Heading2 spn _ -> spn
  Heading3 spn _ -> spn
  Heading4 spn _ -> spn
  Heading5 spn _ -> spn
  Heading6 spn _ -> spn
  CodeBlock spn _ _ -> spn
  Naked spn _ -> spn
  Paragraph spn _ -> spn
  Blockquote spn _ -> spn
  OrderedList spn _ _ -> spn
  UnorderedList spn _ -> spn
  Table spn _ _ -> spn

-- | Replace the annotation of a 'Block', leaving the annotations of the
-- blocks it contains alone.
--
-- @since 0.1.0.0
setBlockSpan :: Span -> Block a -> Block a
setBlockSpan spn = \case
  ThematicBreak _ -> ThematicBreak spn
  Heading1 _ a -> Heading1 spn a
  Heading2 _ a -> Heading2 spn a
  Heading3 _ a -> Heading3 spn a
  Heading4 _ a -> Heading4 spn a
  Heading5 _ a -> Heading5 spn a
  Heading6 _ a -> Heading6 spn a
  CodeBlock _ mi txt -> CodeBlock spn mi txt
  Naked _ a -> Naked spn a
  Paragraph _ a -> Paragraph spn a
  Blockquote _ xs -> Blockquote spn xs
  OrderedList _ w xs -> OrderedList spn w xs
  UnorderedList _ xs -> UnorderedList spn xs
  Table _ ca xs -> Table spn ca xs

-- | Project the annotation of an 'Inline'.
--
-- @since 0.1.0.0
inlineSpan :: Inline -> Span
inlineSpan = \case
  Plain spn _ -> spn
  LineBreak spn -> spn
  Emphasis spn _ -> spn
  Strong spn _ -> spn
  Strikeout spn _ -> spn
  Subscript spn _ -> spn
  Superscript spn _ -> spn
  CodeSpan spn _ -> spn
  Link spn _ _ _ -> spn
  Image spn _ _ _ -> spn

-- | Replace the annotation of an 'Inline', leaving the annotations of the
-- inlines it contains alone.
--
-- @since 0.1.0.0
setInlineSpan :: Span -> Inline -> Inline
setInlineSpan spn = \case
  Plain _ txt -> Plain spn txt
  LineBreak _ -> LineBreak spn
  Emphasis _ xs -> Emphasis spn xs
  Strong _ xs -> Strong spn xs
  Strikeout _ xs -> Strikeout spn xs
  Subscript _ xs -> Subscript spn xs
  Superscript _ xs -> Superscript spn xs
  CodeSpan _ txt -> CodeSpan spn txt
  Link _ xs uri mt -> Link spn xs uri mt
  Image _ xs uri mt -> Image spn xs uri mt

----------------------------------------------------------------------------
-- The transformation monad

-- | The monad a transformation runs in. It gives a transformation a way to
-- report errors, see 'report' and 'abort', and it is a monad transformer,
-- so a transformation that needs to perform effects can have them.
--
-- @since 0.1.0.0
newtype TransT m a
  = TransT (ExceptT Abort (StateT [ParseError Text TransError] m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | The signal 'abort' raises. It carries nothing, because the error that
-- caused it has already been recorded.
data Abort = Abort

instance MonadTrans TransT where
  lift = TransT . lift . lift

-- | The non-transformer version of 'TransT'.
--
-- @since 0.1.0.0
type Trans = TransT Identity

-- | Run a transformation, collecting the errors it reported. Errors
-- accumulate, so an extension that checks something about every node
-- reports every node that fails the check rather than only the first.
--
-- @since 0.1.0.0
runTransT ::
  (Monad m) =>
  -- | The state to resolve the offsets in reported errors against
  PosState Text ->
  -- | The extension to run
  TransT m a ->
  m (Either (ParseErrorBundle Text TransError) a)
runTransT pstate (TransT m) = do
  (r, errs) <- runStateT (runExceptT m) []
  -- A 'ParseErrorBundle' has to be sorted by offset, otherwise
  -- 'errorBundlePretty' cannot go back for the source line of an error that
  -- precedes the one before it and shows the wrong line. A transformation
  -- reports in whatever order suits it, so we sort here. The sort is
  -- stable, so errors at the same offset stay in the order in which they
  -- were reported.
  let sorted = sortOn errorOffset (reverse errs)
  return $ case (NE.nonEmpty sorted, r) of
    (Just errs', _) -> Left (bundle errs')
    (Nothing, Left Abort) -> Left (bundle (unknown :| []))
    (Nothing, Right x) -> Right x
  where
    bundle errs' =
      ParseErrorBundle
        { bundleErrors = errs',
          bundlePosState = pstate
        }
    unknown =
      FancyError 0 (E.singleton (ErrorCustom (TransError "extension failed")))

-- | Report an error at the given 'Span' and carry on. Use this when the
-- rest of the document can still be processed, so that the user is told
-- about every problem at once instead of the first one only.
--
-- @since 0.1.0.0
report :: (Monad m) => Span -> Text -> TransT m ()
report Span {..} msg =
  TransT . lift . modify' $
    (FancyError spanStart (E.singleton (ErrorCustom (TransError msg))) :)

-- | Report an error at the given 'Span' and give up on the document. Errors
-- reported before this one are preserved.
--
-- @since 0.1.0.0
abort :: (Monad m) => Span -> Text -> TransT m a
abort spn msg = report spn msg >> TransT (throwError Abort)

-- | The error a transformation reports, see 'report' and 'abort'. The
-- errors of a transformation are collected in a @'ParseErrorBundle' 'Text'
-- 'TransError'@, the same type the parser produces, so 'errorBundlePretty'
-- renders them against the source of the document just like it renders
-- parse errors.
--
-- @since 0.1.0.0
newtype TransError = TransError Text
  deriving (Eq, Ord, Show, Data, Generic)

instance NFData TransError

instance ShowErrorComponent TransError where
  showErrorComponent (TransError txt) = T.unpack txt

----------------------------------------------------------------------------
-- Rendering

-- | A rendering extension. Unlike transformations, which are applied to a
-- document right away with 'Text.MMark.runTrans' and friends, renders can
-- only be applied while the document is being turned into HTML, so they are
-- collected in a value of this type and handed to 'Text.MMark.render'.
--
-- Note that 'RenderExtension' is an instance of 'Semigroup' and 'Monoid',
-- i.e. you can combine several render extensions into one. Since the
-- @('<>')@ operator is right-associative and 'mconcat' is a right fold
-- under the hood, the expression
--
-- > l <> r
--
-- means that the extension @r@ will be applied before the extension @l@.
--
-- @since 0.1.0.0
data RenderExtension = RenderExtension
  { -- | Block render
    extBlockRender :: Render (Block (Ois, Html ())),
    -- | Inline render
    extInlineRender :: Render Inline
  }

instance Semigroup RenderExtension where
  x <> y =
    RenderExtension
      { extBlockRender = extBlockRender x <> extBlockRender y,
        extInlineRender = extInlineRender x <> extInlineRender y
      }

instance Monoid RenderExtension where
  mempty =
    RenderExtension
      { extBlockRender = mempty,
        extInlineRender = mempty
      }
  mappend = (<>)

-- | An internal type that captures the extensible rendering process we use.
-- 'Render' has a function inside which transforms a rendering function of
-- the type @a -> Html ()@.
--
-- @since 0.0.8.0
newtype Render a = Render
  {runRender :: (a -> Html ()) -> a -> Html ()}

instance Semigroup (Render a) where
  Render f <> Render g = Render (f . g)

instance Monoid (Render a) where
  mempty = Render id
  mappend = (<>)

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
