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
-- markdown document. Note that this transformation can only change
-- 'Block's, not their 'Inline' contents which are denoted here as a
-- universally quantified variable @a@.

blockTrans :: (forall a. Block a -> Block a) -> Extension
blockTrans f = mempty { extBlockTrans = Endo f }

-- | Create an extension that replaces or augments rendering of 'Block's of
-- markdown document. The argument of 'blockRender' will be given rendering
-- function constructed so far @'Block' ('Html' ()) -> 'Html' ()@ as well as
-- actual block to render—@'Block' ('Html' ())@. You can then decide whether
-- to replace\/reuse that function to get the final rendering of the type
-- @'Html' ()@. The argument of 'blockRender' can also be thought of as a
-- function that transforms rendering function constructed so far:
--
-- > (Block (Html ()) -> Html ()) -> (Block (Html ()) -> Html ())

blockRender
  :: ((Block (Html ()) -> Html ()) -> Block (Html ()) -> Html ())
  -> Extension
blockRender f = mempty { extBlockRender = Render f }

-- | Create an extension that performs a transformation on 'Inline'
-- components in entire markdown document.

inlineTrans :: (Inline -> Inline) -> Extension
inlineTrans f = mempty { extInlineTrans = Endo f }

-- | Create an extension that replaces or augments rendering of 'Inline's of
-- markdown document. This works like 'blockRender'.

inlineRender
  :: ((Inline -> Html ()) -> Inline -> Html ())
  -> Extension
inlineRender f = mempty { extInlineRender = Render f }

-- | Create a 'Scanner' from a folding function. Note that the scanning
-- context is passed around being evaluated to weak head normal form on
-- every iteration. If you have a deep data structure in @a@ this may be not
-- enough to fight space leaks though.

scanner :: (a -> Block (NonEmpty Inline) -> a) -> Scanner a
scanner = Scanner
