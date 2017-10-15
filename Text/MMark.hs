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
-- something is not quite right before looking at incorrect (or rather
-- simply unexpected) result of rendering. This feature makes it a good
-- choice for writers and bloggers.
--
-- === MMark and Common Mark
--
-- MMark tries to follow the Common Mark specification as given here:
--
-- <https://github.com/jgm/CommonMark>
--
-- However, due to the fact that we do not allow inputs that do not make
-- sense, MMark obviously can't follow the specification precisely. In
-- particular, parsing of inlines differs considerably from Common Mark. For
-- example, some characters like @*@, @_@, etc. that usually appear in
-- markup in what is called left- and right-flanking delimiter runs are
-- allowed only in those positions:
--
-- > *Something* is not right.
-- > Something __is__ not right.
--
-- This produces a parse error:
--
-- > *Something * is not right.
-- > Something __is __ not right.
--
-- Here is the full list of so-called __markup characters__: @*@, @~@, @_@,
-- @`@, @^@, @[@, @]@. When they appear without escaping, they must form
-- correct markup structures in inlines, otherwise parse errors will be
-- reported.
--
-- The same applies to the syntax of links, images, etc. For example, it's a
-- parse error to put a link into text of another link.
--
-- MMark also does not support hard line breaks represented as double space
-- before newline. Hard line breaks in the form of backslash before newlines
-- are supported.
--
-- Another difference between Common Mark and MMark is that the latter
-- supports more common markdown extensions out-of-the-box. In particular,
-- MMark supports:
--
--     * parsing of optional YAML metadata block (NOT YET)
--     * automatic turning of bare URIs into links (NOT YET)
--     * strikeout using @~~this~~@ syntax
--     * superscript using @^this^@ syntax
--     * subscript using @~this~@ syntax
--     * PHP-style footnotes, e.g. @[^1]@ (NOT YET)
--     * “pipe” tables (as used on GitHub) (NOT YET)
--
-- You do not need to enable or tweak anything for these to work, they are
-- built-in features.
--
-- === How to use the library
--
-- The module is intended to be imported qualified:
--
-- > import Text.MMark (MMark, (.&+))
-- > import qualified Text.MMark as MMark
--
-- Working with MMark happens in three stages:
--
--     1. Parsing of markdown document.
--     2. Applying extensions, which optionally may require scanning of
--        previously parsed document (for example to build a table of
--        contents).
--     3. Rendering of HTML document.
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
-- module, which has some documentation focusing on extension writing.

module Text.MMark
  ( -- * Parsing
    MMark
  , MMarkErr (..)
  , parse
    -- * Extensions
  , Extension
  , useExtension
  , useExtensions
    -- * Scanning
  , Scanner
  , runScanner
  , (.&+)
  , projectYaml
    -- * Rendering
  , render )
where

import Data.Aeson
import Text.MMark.Internal
import Text.MMark.Parser

-- | Extract contents of optional YAML block that may have been parsed.

projectYaml :: MMark -> Maybe Value
projectYaml = mmarkYaml
