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
-- > import Text.MMark.Extension (Bni, Block (..), Inline (..))
-- > import qualified Text.MMark.Extension as Ext
--
-- === Details about extensions
--
-- There are four kinds of extension-producing functions. They correspond
-- internally to four functions that are applied to the parsed document in
-- turn:
--
--     * 'blockTrans' is applied first, as it's quite general and can change
--       block-level structure of document as well as inline-level
--       structure.
--     * 'inlineTrans' is applied to every inline in the document obtained
--       in the previous step.
--     * 'inlineRender' is applied to every inline; this function produces
--       HTML rendition of the inlines and we also preserve the original
--       inline so 'blockRender' can look at it (sometimes it is useful).
--     * 'blockRender' is applied to every block to obtain HTML rendition of
--       the whole document.
--
-- When one combines different extensions, extensions of the same kind get
-- fused together into a single function. This allows for faster processing
-- in the end.

{-# LANGUAGE RankNTypes #-}

module Text.MMark.Extension
  ( -- * Extension construction
    Extension
    -- ** Block-level manipulation
  , Bni
  , Block (..)
  , blockTrans
  , blockRender
  , Ois
  , getOis
    -- ** Inline-level manipulation
  , Inline (..)
  , inlineTrans
  , inlineRender
    -- * Scanner construction
  , scanner
  , scannerM
    -- * Utils
  , asPlainText
  , headerId
  , headerFragment )
where

import Data.Monoid hiding ((<>))
import Lucid
import Text.MMark.Internal
import qualified Control.Foldl as L

-- | Create an extension that performs a transformation on 'Block's of
-- markdown document.

blockTrans :: (Bni -> Bni) -> Extension
blockTrans f = mempty { extBlockTrans = Endo f }

-- | Create an extension that replaces or augments rendering of 'Block's of
-- markdown document. The argument of 'blockRender' will be given the
-- rendering function constructed so far @'Block' ('Ois', 'Html' ()) ->
-- 'Html' ()@ as well as an actual block to render—@'Block' ('Ois', 'Html'
-- ())@. The user can then decide whether to replace\/reuse that function to
-- get the final rendering of the type @'Html' ()@.
--
-- The argument of 'blockRender' can also be thought of as a function that
-- transforms the rendering function constructed so far:
--
-- > (Block (Ois, Html ()) -> Html ()) -> (Block (Ois, Html ()) -> Html ())
--
-- See also: 'Ois' and 'getOis'.

blockRender
  :: ((Block (Ois, Html ()) -> Html ()) -> Block (Ois, Html ()) -> Html ())
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
  -> (a -> Bni -> a)   -- ^ Folding function
  -> L.Fold Bni a      -- ^ Resulting 'L.Fold'
scanner a f = L.Fold f a id
{-# INLINE scanner #-}

-- | Create a 'L.FoldM' from an initial state and a folding function.

scannerM
  :: Monad m
  => m a               -- ^ Initial state
  -> (a -> Bni -> m a) -- ^ Folding function
  -> L.FoldM m Bni a   -- ^ Resulting 'L.FoldM'
scannerM a f = L.FoldM f a return
{-# INLINE scannerM #-}
