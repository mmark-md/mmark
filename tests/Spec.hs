module Main (main) where

import Test.Hspec
import Text.MMark.ExtensionSpec qualified as ExtensionSpec
import Text.MMarkSpec qualified as MMarkSpec

main :: IO ()
main = hspec $ do
  MMarkSpec.spec
  ExtensionSpec.spec
