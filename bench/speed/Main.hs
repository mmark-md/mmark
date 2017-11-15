module Main (main) where

import Criterion.Main
import qualified Data.Text.IO as T
import qualified Text.MMark   as MMark

main :: IO ()
main = defaultMain
  [ bparser "data/bench-paragraph.md"
  ]

----------------------------------------------------------------------------
-- Helpers

bparser
  :: FilePath          -- ^ File from which to load parser's input
  -> Benchmark
bparser path = env (T.readFile path) (bench name . nf p)
  where
    name = "with file: " ++ path
    p    = MMark.parse path
