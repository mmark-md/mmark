module Main (main) where

import Weigh
import qualified Data.Text.IO as T
import qualified Text.MMark   as MMark

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  bparser "data/bench-yaml-block.md"
  bparser "data/bench-thematic-break.md"
  bparser "data/bench-heading.md"
  bparser "data/bench-fenced-code-block.md"
  bparser "data/bench-indented-code-block.md"
  bparser "data/bench-unordered-list.md"
  bparser "data/bench-ordered-list.md"
  bparser "data/bench-blockquote.md"
  bparser "data/bench-paragraph.md"
  bparser "data/comprehensive.md"

----------------------------------------------------------------------------
-- Helpers

bparser
  :: FilePath          -- ^ File from which the input has been loaded
  -> Weigh ()
bparser path = action name (p <$> T.readFile path)
  where
    name = "with file: " ++ path
    p    = MMark.parse path
