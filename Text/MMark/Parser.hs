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

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Void
import Text.MMark.Internal
import Text.Megaparsec

-- | Parse a markdown document in the form of a strict 'Text' value and
-- either report parse errors or return a 'MMark' document. The parser is an
-- efficient parallel parser (meaning it can actually divide the work
-- between several threads) with the ability to report multiple parse
-- errors.

parseMMark :: Text -> Either (NonEmpty (ParseError Char Void)) MMark
parseMMark = undefined -- TODO

-- NOTE We'll need intermediate representation data types for this.
-- NOTE Not sure about Void.
