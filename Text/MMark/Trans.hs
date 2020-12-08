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
-- MMark block\/inline transformation helpers.
module Text.MMark.Trans
  ( applyBlockTrans,
    applyInlineTrans,
  )
where

import Control.Arrow
import Text.MMark.Type

-- | Apply block transformation.
applyBlockTrans :: Monad m => Kleisli m Bni Bni -> Bni -> m Bni
applyBlockTrans trans@(Kleisli f) = \case
  Blockquote xs -> (Blockquote <$> s xs) >>= f
  OrderedList w xs -> (OrderedList w <$> traverse s xs) >>= f
  UnorderedList xs -> (UnorderedList <$> traverse s xs) >>= f
  other -> f other
  where
    s = traverse (applyBlockTrans trans)

-- | Apply inline transformation.
applyInlineTrans :: Monad m => Kleisli m Inline Inline -> Inline -> m Inline
applyInlineTrans trans@(Kleisli f) = \case
  Emphasis xs -> (Emphasis <$> s xs) >>= f
  Strong xs -> (Strong <$> s xs) >>= f
  Strikeout xs -> (Strikeout <$> s xs) >>= f
  Subscript xs -> (Subscript <$> s xs) >>= f
  Superscript xs -> (Superscript <$> s xs) >>= f
  Link xs uri mt -> (Link <$> s xs <*> pure uri <*> pure mt) >>= f
  Image xs uri mt -> (Image <$> s xs <*> pure uri <*> pure mt) >>= f
  other -> f other
  where
    s = traverse (applyInlineTrans trans)
