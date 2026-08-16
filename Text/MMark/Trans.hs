{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Text.MMark.Trans
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Everything needed to write a transformation.
--
-- A transformation is applied to a document with 'Text.MMark.runTrans' or
-- 'Text.MMark.runTransM', so the order in which transformations happen is
-- the order in which you sequence them, and the document that results from
-- one is an ordinary value you can inspect before you apply the next. A
-- transformation runs in the 'TransT' monad, which means it can perform
-- effects and it can 'report' errors against the source of the document.
--
-- Every block and inline carries the 'Span' of the source it derives from,
-- see 'blockSpan' and 'inlineSpan'. That is what lets a transformation
-- report an error that points at the offending markup:
--
-- > brokenLinks :: Inline -> TransT IO Inline
-- > brokenLinks = \case
-- >   l@(Link spn _ uri _) -> do
-- >     ok <- liftIO (checkUri uri)
-- >     unless ok $
-- >       report spn ("cannot reach " <> URI.render uri)
-- >     return l
-- >   other -> return other
--
-- See also: "Text.MMark.Render", which is about changing the way rendering
-- to HTML happens.
--
-- @since 0.0.8.0
module Text.MMark.Trans
  ( -- * Documents
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

    -- * Transformations
    bottomUpBlocks,
    topDownBlocks,
    bottomUpInlines,
    topDownInlines,

    -- * Reporting errors
    TransT,
    Trans,
    TransError (..),
    report,
    abort,

    -- * Utils
    asPlainText,
    headerId,
    headerFragment,
  )
where

import Text.MMark.Internal.Type
import Text.MMark.Util

-- | Apply a function to every block of a block tree, innermost blocks
-- first. A container block is therefore given to the function with its
-- children already transformed.
--
-- @since 0.1.0.0
bottomUpBlocks :: (Monad m) => (Bni -> m Bni) -> Bni -> m Bni
bottomUpBlocks f = go
  where
    go = \case
      Blockquote spn xs -> traverse go xs >>= f . Blockquote spn
      OrderedList spn w xs -> traverse (traverse go) xs >>= f . OrderedList spn w
      UnorderedList spn xs -> traverse (traverse go) xs >>= f . UnorderedList spn
      other -> f other

-- | Apply a function to every block of a block tree, outermost blocks
-- first. A container block is therefore given to the function before its
-- children, and the children that are then visited are the ones the
-- function returned.
--
-- @since 0.1.0.0
topDownBlocks :: (Monad m) => (Bni -> m Bni) -> Bni -> m Bni
topDownBlocks f = go
  where
    go x =
      f x >>= \case
        Blockquote spn xs -> Blockquote spn <$> traverse go xs
        OrderedList spn w xs -> OrderedList spn w <$> traverse (traverse go) xs
        UnorderedList spn xs -> UnorderedList spn <$> traverse (traverse go) xs
        other -> return other

-- | Apply a function to every inline of a block tree, innermost inlines
-- first.
--
-- @since 0.1.0.0
bottomUpInlines :: (Monad m) => (Inline -> m Inline) -> Bni -> m Bni
bottomUpInlines f = bottomUpBlocks (traverse (traverse go))
  where
    go = \case
      Emphasis spn xs -> traverse go xs >>= f . Emphasis spn
      Strong spn xs -> traverse go xs >>= f . Strong spn
      Strikeout spn xs -> traverse go xs >>= f . Strikeout spn
      Subscript spn xs -> traverse go xs >>= f . Subscript spn
      Superscript spn xs -> traverse go xs >>= f . Superscript spn
      Link spn xs uri mt -> traverse go xs >>= \ys -> f (Link spn ys uri mt)
      Image spn xs uri mt -> traverse go xs >>= \ys -> f (Image spn ys uri mt)
      other -> f other

-- | Apply a function to every inline of a block tree, outermost inlines
-- first.
--
-- @since 0.1.0.0
topDownInlines :: (Monad m) => (Inline -> m Inline) -> Bni -> m Bni
topDownInlines f = bottomUpBlocks (traverse (traverse go))
  where
    go x =
      f x >>= \case
        Emphasis spn xs -> Emphasis spn <$> traverse go xs
        Strong spn xs -> Strong spn <$> traverse go xs
        Strikeout spn xs -> Strikeout spn <$> traverse go xs
        Subscript spn xs -> Subscript spn <$> traverse go xs
        Superscript spn xs -> Superscript spn <$> traverse go xs
        Link spn xs uri mt -> (\ys -> Link spn ys uri mt) <$> traverse go xs
        Image spn xs uri mt -> (\ys -> Image spn ys uri mt) <$> traverse go xs
        other -> return other
