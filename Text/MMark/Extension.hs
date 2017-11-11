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
--
-- We suggest using a qualified import, like this:
--
-- > import Text.MMark.Extension (Block (..), Inline (..))
-- > import qualified Text.MMark.Extension as Ext

{-# LANGUAGE RankNTypes #-}

module Text.MMark.Extension
  ( -- * Extension construction
    Extension
    -- ** Block-level manipulation
  , Block (..)
  , blockTrans
  , blockRender
    -- ** Inline-level manipulation
  , Inline (..)
  , inlineTrans
  , inlineRender
    -- * Scanner construction
  , scanner
    -- * Utils
  , asPlainText )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding ((<>))
import Lucid
import Text.MMark.Internal
import qualified Control.Foldl as L

-- | Create an extension that performs a transformation on 'Block's of
-- markdown document. Note that this transformation can only change
-- 'Block's, not their 'Inline' contents which are denoted here as a
-- universally quantified variable @a@.

blockTrans :: (Block (NonEmpty Inline) -> Block (NonEmpty Inline)) -> Extension
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

-- | Create a 'L.Fold' from an initial state and a folding function.

scanner
  :: a                 -- ^ Initial state
  -> (a -> Block (NonEmpty Inline) -> a) -- ^ Folding function
  -> L.Fold (Block (NonEmpty Inline)) a -- ^ Resulting 'L.Fold'
scanner a f = L.Fold f a id
{-# INLINE scanner #-}
