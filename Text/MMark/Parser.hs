-- |
-- Module      :  Text.MMark.Parser
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- MMark parser. You probably want to import "Text.MMark" instead.

module Text.MMark.Parser
  ( parseMMark )
where

import Data.Text (Text)
import Data.Void
import Text.MMark.Internal
import Text.Megaparsec
-- import Text.Megaparsec.Char

-- NOTE We'll need intermediate representation data types for this.

-- NOTE Not sure about Void.

parseMMark :: Text -> Either (ParseError Char Void) MMark
parseMMark = undefined -- TODO
