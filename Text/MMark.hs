-- |
-- Module      :  Text.MMark
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- MMark is a strict markdown processor for writers.
--
-- TODO Proper description pending.

module Text.MMark
  ( MMark
  , Extension
  , Block (..)
  , Inline (..)
  , parseMMark
  , mmarkYaml
  , useExtension
  , useExtensions
  , renderMMark
  , quickMMark )
where

import Data.Aeson
import Data.Text (Text)
import Data.Void
import Text.MMark.Internal
import Text.MMark.Parser
import Text.Megaparsec
import qualified Data.Text.Lazy as TL
import qualified Lucid          as L

mmarkYaml :: MMark -> Maybe Value
mmarkYaml = mmarkYaml_

quickMMark :: [Extension] -> Text -> Either (ParseError Char Void) TL.Text
quickMMark exts =
  fmap (L.renderText . renderMMark . useExtensions exts) . parseMMark
