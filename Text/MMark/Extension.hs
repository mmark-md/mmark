-- |
-- Module      :  Text.MMark.Extension
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides building blocks for extension creation.

{-# LANGUAGE RankNTypes #-}

module Text.MMark.Extension
  ( -- * Extension construction
    Extension
  , Block (..)
  , blockTrans
  , blockRender
  , Inline (..)
  , inlineTrans
  , inlineRender
    -- * Scanner construction
  , Scanner
  , scanner )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding ((<>))
import Lucid
import Text.MMark.Internal

-- | Create an extension that performs a transformation on 'Block's of
-- markdown document.

blockTrans :: (forall a. Block a -> Block a) -> Extension
blockTrans f = mempty { extBlockTrans = Endo f }

-- | Create an extension that replaces or augments rendering of 'Block's of
-- markdown document. The first argument of the function you must provide is
-- the block to render. The second argument is default rendition or
-- rendition we have so far. If you discard the second argument you can
-- replace rendering logic altogether, otherwise you may wrap\/add something
-- at the beginning or end.

blockRender :: (Block (Html ()) -> Html () -> Html ()) -> Extension
blockRender f = mempty { extBlockRender = Render f }

-- | Create an extension that performs a transformation on 'Inline'
-- components in entire markdown document.

inlineTrans :: (Inline -> Inline) -> Extension
inlineTrans f = mempty { extInlineTrans = Endo f }

-- | Create an extension that replaces or augments rendering of 'Inline's of
-- markdown document. This works much like 'blockRender'.

inlineRender :: (Inline -> Html () -> Html ()) -> Extension
inlineRender f = mempty { extInlineRender = Render f }

-- | Create a 'Scanner' from a folding function.

scanner :: (a -> Block (NonEmpty Inline) -> a) -> Scanner a
scanner = Scanner
