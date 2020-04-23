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

import Data.Monoid hiding ((<>))
import Text.MMark.Type

-- | Apply block transformation in the @'Endo' 'Bni'@ form to a block 'Bni'.
applyBlockTrans :: Endo Bni -> Bni -> Bni
applyBlockTrans trans@(Endo f) = \case
  Blockquote xs -> f (Blockquote (s xs))
  OrderedList w xs -> f (OrderedList w (s <$> xs))
  UnorderedList xs -> f (UnorderedList (s <$> xs))
  other -> f other
  where
    s = fmap (applyBlockTrans trans)

-- | Apply inline transformation in the @'Endo' 'Inline'@ form to an
-- 'Inline'.
applyInlineTrans :: Endo Inline -> Inline -> Inline
applyInlineTrans trans@(Endo f) = \case
  Emphasis xs -> f (Emphasis (s xs))
  Strong xs -> f (Strong (s xs))
  Strikeout xs -> f (Strikeout (s xs))
  Subscript xs -> f (Subscript (s xs))
  Superscript xs -> f (Superscript (s xs))
  Link xs uri mt -> f (Link (s xs) uri mt)
  Image xs uri mt -> f (Image (s xs) uri mt)
  other -> f other
  where
    s = fmap (applyInlineTrans trans)
