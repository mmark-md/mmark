module Main (main) where

import Weigh
import qualified Data.Text.IO as T
import qualified Text.MMark   as MMark

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  bparser "data/bench-paragraph.md"

----------------------------------------------------------------------------
-- Helpers

bparser
  :: FilePath          -- ^ File from which the input has been loaded
  -> Weigh ()
bparser path = action name (p <$> T.readFile path)
  where
    name = "with file: " ++ path
    p    = MMark.parse path
