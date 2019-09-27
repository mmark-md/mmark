{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.TestUtils
  ( -- * Document creation and rendering
    mkDoc
  , toText
    -- * Parser expectations
  , (~~->)
  , (~->)
  , (=->)
  , (==->)
  , (##->)
  )
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

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

----------------------------------------------------------------------------
-- Document creation and rendering

-- | Create an 'MMark' document from given input reporting an expectation
-- failure if it cannot be parsed.

mkDoc :: Text -> IO MMark
mkDoc input =
  case MMark.parse "" input of
    Left bundle -> do
      expectationFailure $
        "while parsing a document, parse error(s) occurred:\n" ++
        errorBundlePretty bundle
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
  -> [ParseError Text MMarkErr]
     -- ^ Expected collection of parse errors, in order
  -> Expectation
input ~~-> errs'' =
  case MMark.parse "" input of
    Left bundle' -> unless (bundle == bundle') . expectationFailure $
      "\nthe parser is expected to fail with:\n\n" ++
      errorBundlePretty bundle                 ++
      "\nbut it failed with:\n\n"                  ++
      errorBundlePretty bundle'
    Right x -> expectationFailure $
      "the parser is expected to fail, but it parsed: " ++ show (toText x)
  where
    bundle = ParseErrorBundle
      { bundleErrors = NE.fromList errs''
      , bundlePosState = PosState
        { pstateInput = input
        , pstateOffset = 0
        , pstateSourcePos = initialPos ""
        , pstateTabWidth = mkPos 4
        , pstateLinePrefix = ""
        }
      }

-- | The same as @('~~->')@, but expects only one parse error.

infix 2 ~->

(~->)
  :: Text
     -- ^ Input for parser
  -> ParseError Text MMarkErr
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
    Left bundle -> expectationFailure $
      "the parser is expected to succeed, but it failed with:\n" ++
      errorBundlePretty bundle
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

-- | Just like @('==->')@, but the expectation is set with a 'Lucid.Html'
-- instead. This is useful when there are multiple attributes in the
-- outputted HTML, which can cause GHCJs and GHC to output different strings
-- (as Lucid uses an unordered map underneath for the attributes and GHCJs
-- and GHC have different hashing functions).

(##->)
  :: Text              -- ^ Input for MMark parser
  -> L.Html ()         -- ^ Lucid builder to match against
  -> Expectation
input ##-> expected = input ==-> (TL.toStrict (L.renderText expected) <> "\n")
