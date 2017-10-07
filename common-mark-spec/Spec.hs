{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Monad
import Data.Aeson
import Data.Function (on)
import Data.List (groupBy)
import Data.Text (Text)
import Numeric.Natural
import Test.Hspec hiding (Example)
import Text.MMark
import Text.Megaparsec
import qualified Data.ByteString as B
import qualified Data.Text.Lazy  as TL
import qualified Lucid           as L

data Example = Example
  { exampleMarkdown :: Text
  , exampleHtml     :: TL.Text
  , exampleIndex    :: Natural
  , exampleSection  :: String
  }

instance FromJSON Example where
  parseJSON = withObject "common mark example" $ \o -> do
    exampleMarkdown <- o .: "markdown"
    exampleHtml     <- o .: "html"
    exampleIndex    <- o .: "example"
    exampleSection  <- o .: "section"
    return Example {..}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  r <- runIO (eitherDecodeStrict' <$> B.readFile "common-mark-spec.json")
  case r of
    Left err ->
      it "should load the examples first" $
        expectationFailure ("could not parse the examples: " ++ err)
    Right examples -> do
      let groups = groupBy ((==) `on` exampleSection) examples
      forM_ groups $ \es -> do
        let section = exampleSection (head es)
        describe section $
          forM_ es $ \Example {..} ->
            it ("example " ++ show exampleIndex) $
              case parseMMark "" exampleMarkdown of
                Left err -> expectationFailure . concat $
                  (++ "\n") . parseErrorPretty <$> err
                Right mmark ->
                  L.renderText (renderMMark mmark) `shouldBe` exampleHtml
