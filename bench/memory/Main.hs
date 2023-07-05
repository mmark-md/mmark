module Main (main) where

import Data.Text.IO qualified as T
import Text.MMark qualified as MMark
import Weigh

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  bparser "data/bench-yaml-block.md"
  bparser "data/bench-thematic-break.md"
  bparser "data/bench-heading.md"
  bparser "data/bench-fenced-code-block.md"
  bparser "data/bench-indented-code-block.md"
  bparser "data/bench-intensive-emphasis.md"
  bparser "data/bench-unordered-list.md"
  bparser "data/bench-ordered-list.md"
  bparser "data/bench-blockquote.md"
  bparser "data/bench-paragraph.md"
  bparser "data/table.md"
  bparser "data/comprehensive.md"

----------------------------------------------------------------------------
-- Helpers

bparser ::
  -- | File from which the input has been loaded
  FilePath ->
  Weigh ()
bparser path = action name (p <$> T.readFile path)
  where
    name = "with file: " ++ path
    p = MMark.parse path
