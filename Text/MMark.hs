{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Text.MMark
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- MMark (read “em-mark”) is a strict markdown processor for writers.
-- “Strict” means that not every input is considered valid markdown document
-- and parse errors are possible and even desirable, because they allow us
-- to spot markup issues without searching for them in rendered document. If
-- a markdown document passes the MMark parser, then it'll likely produce
-- HTML without quirks. This feature makes it a good choice for writers and
-- bloggers.
--
-- === MMark and Common Mark
--
-- MMark mostly tries to follow the Common Mark specification as given here:
--
-- <https://spec.commonmark.org/0.28/>
--
-- However, due to the fact that we do not allow inputs that do not make
-- sense, and also try to guard against common mistakes (like writing @##My
-- header@ and having it rendered as a paragraph starting with hashes) MMark
-- obviously can't follow the specification precisely. In particular,
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
--     * pipe tables (as on GitHub)
--
-- One does not need to enable or tweak anything for these to work, they are
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
-- === “Getting started” example
--
-- Here is a complete example of a program that reads a markdown file named
-- @\"input.md\"@ and outputs an HTML file named @\"output.html\"@:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import qualified Data.Text.IO      as T
-- > import qualified Data.Text.Lazy.IO as TL
-- > import qualified Lucid             as L
-- > import qualified Text.MMark        as MMark
-- > import qualified Text.Megaparsec   as M
-- >
-- > main :: IO ()
-- > main = do
-- >   let input = "input.md"
-- >   txt <- T.readFile input -- (1)
-- >   case MMark.parse input txt of -- (2)
-- >     Left bundle -> putStrLn (M.errorBundlePretty bundle) -- (3)
-- >     Right r -> TL.writeFile "output.html" -- (6)
-- >       . L.renderText -- (5)
-- >       . MMark.render -- (4)
-- >       $ r
--
-- Let's break it down:
--
--     1. We read a source markdown file as strict 'Text'.
--     2. The source is fed into the 'parse' function which does the
--        parsing. It can either fail with a collection of parse errors
--        or succeed returning a value of the opaque 'MMark' type.
--     3. If parsing fails, we pretty-print the parse errors with
--        'Text.Megaparsec.errorBundlePretty'.
--     4. Then we just render the document with 'render' first to Lucid's
--        @'Lucid.Html' ()@.
--     5. …and then to lazy 'Data.Text.Lazy.Text' with 'Lucid.renderText'.
--     6. Finally we write the result as @\"output.html\"@.
--
-- === Other modules of interest
--
-- The "Text.MMark" module contains all the “core” functionality one may
-- need. However, one of the main selling points of MMark is that it's
-- possible to write your own extensions which stay highly composable (if
-- done right), so proliferation of third-party extensions is to be expected
-- and encouraged. To write an extension of your own import the
-- "Text.MMark.Extension" module, which has some documentation focusing on
-- extension writing.
module Text.MMark
  ( -- * Parsing
    MMark,
    MMarkM,
    MMarkErr (..),
    parse,
    parseM,

    -- * Extensions
    Extension,
    ExtensionM,
    useExtension,
    useExtensions,

    -- * Scanning
    runScanner,
    runScannerM,
    projectYaml,

    -- * Rendering
    render,
  )
where

import qualified Control.Foldl as L
import Data.Aeson
import Text.MMark.Parser (MMarkErr (..), parse, parseM)
import Text.MMark.Render (render)
import Text.MMark.Type

----------------------------------------------------------------------------
-- Extensions

-- | Apply an 'Extension' to an 'MMark' document. The order in which you
-- apply 'Extension's /does matter/. Extensions you apply first take effect
-- first. The extension system is designed in such a way that in many cases
-- the order doesn't matter, but sometimes the difference is important.
useExtension :: Monad m => ExtensionM m -> MMarkM m -> MMarkM m
useExtension ext mmark =
  mmark {mmarkExtension = ext <> mmarkExtension mmark}

-- | Apply several 'Extension's to an 'MMark' document.
--
-- This is a simple shortcut:
--
-- > useExtensions exts = useExtension (mconcat exts)
--
-- As mentioned in the docs for 'useExtension', the order in which you apply
-- extensions matters. Extensions closer to beginning of the list are
-- applied later, i.e. the last extension in the list is applied first.
useExtensions :: Monad m => [ExtensionM m] -> MMarkM m -> MMarkM m
useExtensions exts = useExtension (mconcat exts)

----------------------------------------------------------------------------
-- Scanning

-- | Scan an 'MMark' document efficiently in one pass. This uses the
-- excellent 'L.Fold' type, which see.
--
-- Take a look at the "Text.MMark.Extension" module if you want to create
-- scanners of your own.
runScanner ::
  -- | Document to scan
  MMark ->
  -- | 'L.Fold' to use
  L.Fold Bni a ->
  -- | Result of scanning
  a
runScanner MMarkM {..} f = L.fold f mmarkBlocks

-- | Like 'runScanner', but allows us to run scanners with monadic context.
--
-- To bring 'L.Fold' and 'L.FoldM' types to the “least common denominator”
-- use 'L.generalize' and 'L.simplify'.
--
-- @since 0.0.2.0
runScannerM ::
  Monad m =>
  -- | Document to scan
  MMark ->
  -- | 'L.FoldM' to use
  L.FoldM m Bni a ->
  -- | Result of scanning
  m a
runScannerM MMarkM {..} f = L.foldM f mmarkBlocks

-- | Extract contents of an optional YAML block that may have been parsed.
projectYaml :: MMarkM m -> Maybe Value
projectYaml = mmarkYaml
