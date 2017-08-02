-- |
-- Module      :  Text.MMark
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- MMark (read “em-mark”) is a strict markdown processor for writers.
-- “Strict” means that not every input is considered valid markdown and
-- parse errors are possible and even desirable, so one can know that
-- something is not quite right before looking at an incorrect (or rather
-- simply unexpected) result of rendering. This feature makes it a good
-- choice for writers and bloggers.
--
-- === MMark and Common Mark
--
-- MMark is mostly compatible with the Common Mark specification as given
-- here: <https://github.com/jgm/CommonMark>. However, in some cases it
-- rejects (arguably) questionable input and reports parse errors. Here is
-- the full list of cases when MMark diverges from Common Mark:
--
--     * TODO
--
-- Another difference between Common Mark and MMark is that the latter
-- supports more common markdown extensions out-of-the-box. In particular,
-- MMark supports:
--
--     * parsing of optional YAML metadata block
--     * automatic turning of bare URIs into links
--     * strikeout using @~~this~~@ syntax
--     * superscript using @^this^@ syntax
--     * subscript using @~this~@ syntax
--     * PHP-style footnotes, e.g. @[^1]@
--     * “pipe” tables (as used on GitHub)
--
-- You do not need to enable or tweak anything for these to work, they are
-- built-in features.
--
-- === How to use the library
--
-- Working with MMark happens in three stages:
--
--     1. Parsing.
--     2. Applying extensions, which optionally may require scanning of
--        previously parsed document (for example to build a table of
--        contents).
--     3. Rendering.
--
-- The structure of the documentation below corresponds to these stages and
-- should clarify the details.
--
-- === Other modules of interest
--
-- This module contains all the “core” functionality you may need. However,
-- one of the main selling points of MMark is that it's possible to write
-- your own extensions which stay highly composable (if done right), so
-- proliferation of third-party extensions is to be expected and encouraged.
-- To write an extension of your own import the "Text.MMark.Extension"
-- module, which has its own documentation focusing on extension writing.
--
-- Finally, the library comes with a collection of extensions that can be
-- found in the "Text.MMark.Extension.Common".

module Text.MMark
  ( -- * Parsing
    MMark
  , parseMMark
    -- * Extensions and scanning
  , Extension
  , useExtension
  , useExtensions
  , Scanner
  , runScanner
  , mmarkYaml
    -- * Rendering
  , renderMMark )
where

import Data.Aeson
import Text.MMark.Internal
import Text.MMark.Parser

-- | Extract contents of optional YAML block that may have been parsed.

mmarkYaml :: MMark -> Maybe Value
mmarkYaml = mmarkYaml_
