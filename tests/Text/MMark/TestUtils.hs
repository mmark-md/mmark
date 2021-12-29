{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.TestUtils
  ( -- * Document creation and rendering
    mkDoc,
    mkDocM,
    toText,
    toTextM,

    -- * Parser expectations
    (~~->),
    (~->),
    (=->),
    (==->),
    (##->),
  )
where

import Control.Monad
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Lucid as L
import Test.Hspec
import Text.MMark (MMark, MMarkErr, MMarkM)
import qualified Text.MMark as MMark
import Text.Megaparsec

----------------------------------------------------------------------------
-- Document creation and rendering

-- | Create an 'MMark' document from the given input reporting an
-- expectation failure if it cannot be parsed.
mkDoc :: Text -> IO MMark
mkDoc = mkDocM

mkDocM :: Monad m => Text -> IO (MMarkM m)
mkDocM input =
  case MMark.parseM "" input of
    Left bundle -> do
      expectationFailure $
        "while parsing a document, parse error(s) occurred:\n"
          ++ errorBundlePretty bundle
      undefined
    Right x -> return x

-- | Render an 'MMark' document to 'Text'.
toText :: MMark -> Text
toText = TL.toStrict . L.renderText . MMark.render

-- | Render an 'MMark' document to 'm Text'.
toTextM :: Monad m => MMarkM m -> m Text
toTextM = fmap TL.toStrict . L.renderTextT . MMark.render

----------------------------------------------------------------------------
-- Parser expectations

-- | Create an expectation that parser should fail producing a certain
-- collection of 'ParseError's.
infix 2 ~~->

(~~->) ::
  -- | Input for parser
  Text ->
  -- | Expected collection of parse errors, in order
  [ParseError Text MMarkErr] ->
  Expectation
input ~~-> errs'' =
  case MMark.parse "" input of
    Left bundle' ->
      unless (bundle == bundle') . expectationFailure $
        "\nthe parser is expected to fail with:\n\n"
          ++ errorBundlePretty bundle
          ++ "\nbut it failed with:\n\n"
          ++ errorBundlePretty bundle'
    Right x ->
      expectationFailure $
        "the parser is expected to fail, but it parsed: " ++ show (toText x)
  where
    bundle =
      ParseErrorBundle
        { bundleErrors = NE.fromList errs'',
          bundlePosState =
            PosState
              { pstateInput = input,
                pstateOffset = 0,
                pstateSourcePos = initialPos "",
                pstateTabWidth = mkPos 4,
                pstateLinePrefix = ""
              }
        }

-- | The same as @('~~->')@, but expects only one parse error.
infix 2 ~->

(~->) ::
  -- | Input for parser
  Text ->
  -- | Expected parse error to compare with
  ParseError Text MMarkErr ->
  Expectation
input ~-> err = input ~~-> [err]

-- | Test parser and render by specifying input for parser and expected
-- output of render.
infix 2 =->

(=->) ::
  -- | Input for MMark parser
  Text ->
  -- | Output of render to match against
  Text ->
  Expectation
input =-> expected =
  case MMark.parse "" input of
    Left bundle ->
      expectationFailure $
        "the parser is expected to succeed, but it failed with:\n"
          ++ errorBundlePretty bundle
    Right factual -> toText factual `shouldBe` expected

-- | Just like @('=->')@, but also appends newline to given input and tries
-- with that as well.
infix 9 ==->

(==->) ::
  -- | Input for MMark parser
  Text ->
  -- | Output of render to match against
  Text ->
  Expectation
input ==-> expected = do
  input =-> expected
  mappend input "\n" =-> expected

-- | Just like @('==->')@, but the expectation is set with a 'Lucid.Html'
-- instead. This is useful when there are multiple attributes in the
-- outputted HTML, which can cause GHCJs and GHC to output different strings
-- (as Lucid uses an unordered map underneath for the attributes and GHCJs
-- and GHC have different hashing functions).
(##->) ::
  -- | Input for MMark parser
  Text ->
  -- | Lucid builder to match against
  L.Html () ->
  Expectation
input ##-> expected = input ==-> (TL.toStrict (L.renderText expected) <> "\n")
