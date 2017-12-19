{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.TestUtils
  ( -- * Document creation and rendering
    mkDoc
  , toText
    -- * Parser expectations
  , (~~->)
  , (~->)
  , (=->)
  , (==->) )
where

import Control.Monad
import Data.Text (Text)
import Test.Hspec
import Text.MMark (MMark, MMarkErr)
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy     as TL
import qualified Lucid              as L
import qualified Text.MMark         as MMark

----------------------------------------------------------------------------
-- Document creation and rendering

-- | Create an 'MMark' document from given input reporting an expectation
-- failure if it cannot be parsed.

mkDoc :: Text -> IO MMark
mkDoc input =
  case MMark.parse "" input of
    Left errs -> do
      expectationFailure $
        "while parsing a document, parse error(s) occurred:\n" ++
        MMark.parseErrorsPretty input errs
      undefined
    Right x -> return x

-- | Render an 'MMark' document to 'Text'.

toText :: MMark -> Text
toText = TL.toStrict . L.renderText . MMark.render

----------------------------------------------------------------------------
-- Parser expectations

-- | Create an expectation that parser should fail producing a certain
-- collection of 'ParseError's.

infix 2 ~~->

(~~->)
  :: Text
     -- ^ Input for parser
  -> [ParseError Char MMarkErr]
     -- ^ Expected collection of parse errors, in order
  -> Expectation
input ~~-> errs'' =
  case MMark.parse "" input of
    Left errs' -> unless (errs == errs') . expectationFailure $
      "the parser is expected to fail with:\n" ++
      MMark.parseErrorsPretty input errs       ++
      "but it failed with:\n"                  ++
      MMark.parseErrorsPretty input errs'
    Right x -> expectationFailure $
      "the parser is expected to fail, but it parsed: " ++ show (toText x)
  where
    errs = NE.fromList errs''

-- | The same as @('~~->')@, but expects only one parse error.

infix 2 ~->

(~->)
  :: Text
     -- ^ Input for parser
  -> ParseError Char MMarkErr
     -- ^ Expected parse error to compare with
  -> Expectation
input ~-> err = input ~~-> [err]

-- | Test parser and render by specifying input for parser and expected
-- output of render.

infix 2 =->

(=->)
  :: Text              -- ^ Input for MMark parser
  -> Text              -- ^ Output of render to match against
  -> Expectation
input =-> expected =
  case MMark.parse "" input of
    Left errs -> expectationFailure $
      "the parser is expected to succeed, but it failed with:\n" ++
      MMark.parseErrorsPretty input errs
    Right factual -> toText factual `shouldBe` expected

-- | Just like @('=->')@, but also appends newline to given input and tries
-- with that as well.

infix ==->

(==->)
  :: Text              -- ^ Input for MMark parser
  -> Text              -- ^ Output of render to match against
  -> Expectation
input ==-> expected = do
  input              =-> expected
  mappend input "\n" =-> expected
