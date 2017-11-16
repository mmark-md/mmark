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
-- “Strict” means that not every input is considered a valid markdown
-- document and parse errors are possible and even desirable, because they
-- allow to spot markup issues without searching for them in rendered
-- document. If a markdown document passes MMark parser, then it'll most
-- certainly produce HTML without quirks. This feature makes it a good
-- choice for writers and bloggers.
--
-- === MMark and Common Mark
--
-- MMark mostly tries to follow the Common Mark specification as given here:
--
-- <https://github.com/jgm/CommonMark>
--
-- However, due to the fact that we do not allow inputs that do not make
-- sense, and also try to guard against common silly mistakes (like writing
-- @##My header@ and having it rendered as a paragraph starting with hashes)
-- MMark obviously can't follow the specification precisely. In particular,
-- parsing of inlines differs considerably from Common Mark.
--
-- Another difference between Common Mark and MMark is that the latter
-- supports more (pun alert) common markdown extensions out-of-the-box. In
-- particular, MMark supports:
--
--     * parsing of an optional YAML block
--     * strikeout using @~~this~~@ syntax
--     * superscript using @^this^@ syntax
--     * subscript using @~this~@ syntax
--     * automatic assignment of ids to headers
--     * PHP-style footnotes, e.g. @[^1]@ (NOT YET)
--     * “pipe” tables (as used on GitHub) (NOT YET)
--
-- One do not need to enable or tweak anything for these to work, they are
-- built-in features.
--
-- The readme contains a more detailed description of differences between
-- Common Mark and MMark.
--
-- === How to use the library
--
-- The module is intended to be imported qualified:
--
-- > import Text.MMark (MMark)
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
  , parseErrorsPretty
    -- * Extensions
  , Extension
  , useExtension
  , useExtensions
    -- * Scanning
  , runScanner
  , projectYaml
    -- * Rendering
  , render )
where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Text.MMark.Internal
import Text.MMark.Parser
import Text.Megaparsec (ParseError (..), parseErrorPretty_, mkPos)

-- | Extract contents of optional YAML block that may have been parsed.

projectYaml :: MMark -> Maybe Value
projectYaml = mmarkYaml

-- | Pretty-print a collection of parse errors returned from 'parse'.
--
-- __Pro tip__: if you would like to pretty-print a single 'ParseError', use
-- @'parseErrorPretty_' ('mkPos' 4)@, because Common Mark suggests that we
-- should assume tab width 4, and that's what we do in the parser.

parseErrorsPretty
  :: Text              -- ^ Original input for parser
  -> NonEmpty (ParseError Char MMarkErr) -- ^ Collection of parse errors
  -> String            -- ^ Result of pretty-printing
parseErrorsPretty input = concatMap (parseErrorPretty_ (mkPos 4) input)
