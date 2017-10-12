module Text.MMark.TestUtils
  ( -- * Document creation and rendering
    mkDoc
  , toText
    -- * Parser expectations
  , shouldFailWith
  , shouldProduce )
where

import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Void
import Test.Hspec
import Text.MMark
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as TL
import qualified Lucid              as L

----------------------------------------------------------------------------
-- Document creation and rendering

-- | Create an 'MMark' document from given input reporting an expectation
-- failure if it cannot be parsed.

mkDoc :: Text -> IO MMark
mkDoc input =
  case parseMMark "" input of
    Left errs -> do
      expectationFailure $
        "while parsing a document, parse error(s) occurred:\n" ++
        showParseErrors input errs
      undefined
    Right x -> return x

-- | Render an 'MMark' document to 'Text'.

toText :: MMark -> Text
toText = TL.toStrict . L.renderText . renderMMark

----------------------------------------------------------------------------
-- Parser expectations

-- | Create an expectation that parser should fail producing a certain
-- collection of 'ParseError's.

shouldFailWith
  :: Text              -- ^ Input for parser
  -> [ParseError Char Void] -- ^ Expected collection of parse errors, in order
  -> Expectation
input `shouldFailWith` errs'' = case parseMMark "" input of
  Left errs' -> unless (errs == errs') . expectationFailure $
    "the parser is expected to fail with:\n" ++
    showParseErrors input errs               ++
    "but it failed with:\n"                  ++
    showParseErrors input errs'
  Right x -> expectationFailure $
    "the parser is expected to fail, but it parsed: " ++ T.unpack (toText x)
  where
    errs = NE.fromList errs''

-- | Test parser and render by specifying input for parser and expected
-- output of render.

shouldProduce
  :: Text              -- ^ Input for MMark parser
  -> Text              -- ^ Output of render to match against
  -> Expectation
shouldProduce input expected = case parseMMark "" input of
  Left errs -> expectationFailure $
    "the parser is expected to succeed, but it failed with:\n" ++
    showParseErrors input errs
  Right factual -> toText factual `shouldBe` expected

----------------------------------------------------------------------------
-- Helpers

-- | Render a non-empty collection of parse errors.

showParseErrors
  :: Text              -- ^ Original parser input
  -> NonEmpty (ParseError Char Void) -- ^ Collection of parse errors to show
  -> String            -- ^ Rendered errors
showParseErrors input = concatMap (parseErrorPretty' input)
