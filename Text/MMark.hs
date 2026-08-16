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
-- “Strict” means that not every input is considered a valid markdown
-- document and parse errors are possible and even desirable, because they
-- allow us to spot markup issues without searching for them in the rendered
-- document. If a markdown document passes the MMark parser, then it'll
-- likely produce HTML without quirks. This feature makes it a good choice
-- for writers and bloggers.
--
-- === MMark and CommonMark
--
-- MMark mostly tries to follow the CommonMark specification as given here:
--
-- <https://spec.commonmark.org/0.31.2/>
--
-- However, due to the fact that we do not allow inputs that do not make
-- sense, and also try to guard against common mistakes (like writing @##My
-- header@ and having it rendered as a paragraph starting with hashes), MMark
-- obviously can't follow the specification precisely. In particular,
-- parsing of inlines is stricter than CommonMark.
--
-- Another difference between CommonMark and MMark is that the latter
-- supports more (pun alert) common markdown extensions out of the box. In
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
-- CommonMark and MMark.
--
-- === How to use the library
--
-- The module is intended to be imported qualified:
--
-- > import Text.MMark (MMark)
-- > import qualified Text.MMark as MMark
--
-- Working with MMark happens in four stages:
--
--     1. Parsing of a markdown document.
--     2. Scanning of the parsed document, which is optional and collects
--        whatever a transformation may need to know about the document as a
--        whole (for example to build a table of contents).
--     3. Applying transformations. A transformation is applied right away
--        and can fail, so this stage produces either a new document or a
--        collection of errors to report.
--     4. Rendering of an HTML document, optionally augmented by render
--        extensions.
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
-- >       . MMark.render mempty -- (4)
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
--     4. We render the document with 'render' first to Lucid's
--        @'Lucid.Html' ()@, passing it the render extensions to use, or
--        'mempty' when there are none.
--     5. Then we render to lazy 'Data.Text.Lazy.Text' with 'Lucid.renderText'.
--     6. Finally we write the result as @\"output.html\"@.
--
-- === Other modules of interest
--
-- The "Text.MMark" module contains all the “core” functionality one may
-- need. However, one of the main selling points of MMark is that it's
-- possible to write your own extensions, so proliferation of third-party
-- extensions is to be expected and encouraged. To write an extension of
-- your own import "Text.MMark.Trans" if it rewrites the document, or
-- "Text.MMark.Render" if it changes the way the document is rendered. Both
-- modules have documentation focusing on extension writing.
module Text.MMark
  ( -- * Parsing
    MMark,
    MMarkErr (..),
    parse,

    -- * Scanning
    scanner,
    scannerM,
    runScanner,
    runScannerM,
    projectYaml,

    -- * Transformation
    TransT,
    Trans,
    TransError (..),
    runTrans,
    runTransM,
    runCheck,
    runCheckM,

    -- * Rendering
    RenderExtension,
    render,
  )
where

import Control.Foldl qualified as L
import Data.Aeson
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Text.MMark.Internal.Type
import Text.MMark.Parser (MMarkErr (..), parse)
import Text.MMark.Render (render)
import Text.Megaparsec (ParseErrorBundle)

----------------------------------------------------------------------------
-- Scanning

-- | Create a 'L.Fold' from an initial state and a folding function.
scanner ::
  -- | Initial state
  a ->
  -- | Folding function
  (a -> Bni -> a) ->
  -- | Resulting 'L.Fold'
  L.Fold Bni a
scanner a f = L.Fold f a id

-- | Create a 'L.FoldM' from an initial state and a folding function
-- operating in monadic context.
--
-- @since 0.0.2.0
scannerM ::
  (Monad m) =>
  -- | Initial state
  m a ->
  -- | Folding function
  (a -> Bni -> m a) ->
  -- | Resulting 'L.FoldM'
  L.FoldM m Bni a
scannerM a f = L.FoldM f a return

-- | Scan an 'MMark' document efficiently in one pass. This uses the
-- excellent 'L.Fold' type, which see.
--
-- __Note__: the type of this function changed in /0.1.0.0/.
runScanner ::
  -- | 'L.Fold' to use
  L.Fold Bni a ->
  -- | Document to scan
  MMark ->
  -- | Result of scanning
  a
runScanner f MMark {..} = L.fold f mmarkBlocks

-- | Like 'runScanner', but allows us to run scanners with monadic context.
--
-- To bring 'L.Fold' and 'L.FoldM' types to the “least common denominator”
-- use 'L.generalize' and 'L.simplify'.
--
-- __Note__: the type of this function changed in /0.1.0.0/.
--
-- @since 0.0.2.0
runScannerM ::
  (Monad m) =>
  -- | 'L.FoldM' to use
  L.FoldM m Bni a ->
  -- | Document to scan
  MMark ->
  -- | Result of scanning
  m a
runScannerM f MMark {..} = L.foldM f mmarkBlocks

-- | Extract contents of an optional YAML block that may have been parsed.
projectYaml :: MMark -> Maybe Value
projectYaml = mmarkYaml

----------------------------------------------------------------------------
-- Transformation

-- | Apply a pure transformation to an 'MMark' document, see 'runTransM'.
--
-- @since 0.1.0.0
runTrans ::
  -- | The transformation to apply to every top-level block
  (Bni -> Trans Bni) ->
  -- | Document to transform
  MMark ->
  -- | The transformed document, or the errors the transformation reported
  Either (ParseErrorBundle Text TransError) MMark
runTrans f = runIdentity . runTransM f

-- | Apply a transformation to an 'MMark' document, possibly performing
-- effects along the way.
--
-- The function is applied to every top-level block of the document as it
-- is; to reach the blocks and inlines nested inside of those, wrap it in
-- one of the transformations from "Text.MMark.Trans", for example
-- 'Text.MMark.Trans.bottomUpBlocks' or
-- 'Text.MMark.Trans.bottomUpInlines'.
--
-- Several transformations compose with @('Control.Monad.>=>')@ into one,
-- which is then applied in a single pass, and the errors all of them
-- reported are reported together:
--
-- > let trans = bottomUpInlines checkLinks >=> bottomUpBlocks numberHeadings
-- > r <- runTransM trans doc
-- > case r of
-- >   Left errs -> putStr (errorBundlePretty errs)
-- >   Right doc' -> TL.putStr (renderText (render mempty doc'))
--
-- A transformation that 'Text.MMark.Trans.report's an error does not stop
-- the ones that follow it, so a document with several problems in it names
-- them all at once. A transformation that 'Text.MMark.Trans.abort's gives
-- up on the rest of the document, but the errors that were reported before
-- it are still returned.
--
-- @since 0.1.0.0
runTransM ::
  (Monad m) =>
  -- | The transformation to apply to every top-level block
  (Bni -> TransT m Bni) ->
  -- | Document to transform
  MMark ->
  -- | The transformed document, or the errors the transformation reported
  m (Either (ParseErrorBundle Text TransError) MMark)
runTransM f mmark@MMark {..} =
  fmap (fmap replaceBlocks) . runTransT mmarkSource $
    traverse f mmarkBlocks
  where
    replaceBlocks bs = mmark {mmarkBlocks = bs}

-- | Run a pure check against an 'MMark' document, see 'runCheckM'.
--
-- @since 0.1.0.0
runCheck ::
  -- | The check to run
  Trans a ->
  -- | Document to resolve the reported positions against
  MMark ->
  -- | The result of the check, or the errors it reported
  Either (ParseErrorBundle Text TransError) a
runCheck t = runIdentity . runCheckM t

-- | Run a check against an 'MMark' document, possibly performing effects
-- along the way.
--
-- Unlike 'runTransM', which is given a function and applies it to every
-- top-level block, this runs the computation once and leaves the document
-- alone. Use it for a check that concerns the document as a whole, so that
-- the check does not have to pretend to be a transformation of a block it
-- has no interest in:
--
-- > let fns = MMark.runScanner footnoteScanner doc
-- > case MMark.runCheck (validateFootnotes fns) doc of
-- >   Left errs -> putStrLn (errorBundlePretty errs)
-- >   Right () -> …
--
-- The document is only needed to turn the offsets of the reported spans
-- back into lines and columns.
--
-- @since 0.1.0.0
runCheckM ::
  (Monad m) =>
  -- | The check to run
  TransT m a ->
  -- | Document to resolve the reported positions against
  MMark ->
  -- | The result of the check, or the errors it reported
  m (Either (ParseErrorBundle Text TransError) a)
runCheckM t MMark {..} = runTransT mmarkSource t
