{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Text.MMark.Parser.Internal
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- An internal module that builds a framework that the "Text.MMark.Parser"
-- module uses.
module Text.MMark.Parser.Internal
  ( -- * Block-level parser monad
    BParser,
    runBParser,
    isNakedAllowed,
    refLevel,
    subEnv,
    registerReference,

    -- * Inline-level parser monad
    IParser,
    runIParser,
    disallowEmpty,
    isEmptyAllowed,
    disallowLinks,
    isLinksAllowed,
    disallowImages,
    isImagesAllowed,
    getLastChar,
    lastChar,
    lookupReference,
    Isp (..),
    CharType (..),

    -- * Reference and footnote definitions
    Defs,

    -- * Other
    MMarkErr (..),
  )
where

import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text.Metrics (damerauLevenshteinNorm)
import Lens.Micro (Lens', over, set, (.~), (^.))
import Lens.Micro.Extras (view)
import Text.MMark.Parser.Internal.Type
import Text.Megaparsec hiding (State)
import Text.Megaparsec qualified as M
import Text.URI (URI)

----------------------------------------------------------------------------
-- Block-level parser monad

-- | Block-level parser type.
type BParser a = ParsecT MMarkErr Text (State BlockState) a

-- | Run a computation in the 'BParser' monad.
runBParser ::
  -- | The parser to run
  BParser a ->
  -- | File name (only to be used in error messages), may be empty
  FilePath ->
  -- | Input to parse
  Text ->
  -- | Result of parsing
  Either (ParseErrorBundle Text MMarkErr) (a, Defs)
runBParser p file input =
  case runState (snd <$> runParserT' p st) initialBlockState of
    (Left bundle, _) -> Left bundle
    (Right x, st') -> Right (x, st' ^. bstDefs)
  where
    st = mkInitialState file input 0

-- | Ask whether naked paragraphs are allowed in this context.
isNakedAllowed :: BParser Bool
isNakedAllowed = gets (^. bstAllowNaked)

-- | Lookup current reference indentation level.
refLevel :: BParser Pos
refLevel = gets (^. bstRefLevel)

-- | Execute 'BParser' computation with modified environment.
subEnv ::
  -- | Whether naked paragraphs should be allowed
  Bool ->
  -- | Reference indentation level
  Pos ->
  -- | The parser we want to set the environment for
  BParser a ->
  -- | The resulting parser
  BParser a
subEnv allowNaked rlevel =
  locally bstAllowNaked allowNaked
    . locally bstRefLevel rlevel

-- | Register a reference (link\/image) definition.
registerReference ::
  -- | Reference name
  Text ->
  -- | Reference 'URI' and optional title
  (URI, Maybe Text) ->
  -- | 'True' if there is a conflicting definition
  BParser Bool
registerReference = registerGeneric referenceDefs

-- | A generic function for registering definitions in 'BParser'.
registerGeneric ::
  -- | How to access the definition map
  Lens' Defs (HashMap DefLabel a) ->
  -- | Definition name
  Text ->
  -- | Data
  a ->
  -- | 'True' if there is a conflicting definition
  BParser Bool
registerGeneric l name a = do
  let dlabel = mkDefLabel name
  defs <- gets (^. bstDefs . l)
  if HM.member dlabel defs
    then return True
    else do
      modify' $ over (bstDefs . l) (HM.insert dlabel a)
      return False

----------------------------------------------------------------------------
-- Inline-level parser monad

-- | Inline-level parser type.
type IParser a = StateT InlineState (Parsec MMarkErr Text) a

-- | Run a computation in the 'IParser' monad.
runIParser ::
  -- | Reference and footnote definitions obtained as a result of
  -- block-level parsing
  Defs ->
  -- | The parser to run
  IParser a ->
  -- | Input for the parser
  Isp ->
  -- | Result of parsing
  Either (ParseError Text MMarkErr) a
runIParser _ _ (IspError err) = Left err
runIParser defs p (IspSpan offset input) =
  first (NE.head . bundleErrors) (snd (runParser' (evalStateT p ist) pst))
  where
    ist = initialInlineState & istDefs .~ defs
    pst = mkInitialState "" input offset

-- | Disallow parsing of empty inlines.
disallowEmpty :: IParser a -> IParser a
disallowEmpty = locally istAllowEmpty False

-- | Ask whether parsing of empty inlines is allowed.
isEmptyAllowed :: IParser Bool
isEmptyAllowed = gets (view istAllowEmpty)

-- | Disallow parsing of links.
disallowLinks :: IParser a -> IParser a
disallowLinks = locally istAllowLinks False

-- | Ask whether parsing of links is allowed.
isLinksAllowed :: IParser Bool
isLinksAllowed = gets (view istAllowLinks)

-- | Disallow parsing of images.
disallowImages :: IParser a -> IParser a
disallowImages = locally istAllowImages False

-- | Ask whether parsing of images is allowed.
isImagesAllowed :: IParser Bool
isImagesAllowed = gets (view istAllowImages)

-- | Get type of the last parsed character.
getLastChar :: IParser CharType
getLastChar = gets (view istLastChar)

-- | Register type of the last parsed character.
lastChar :: CharType -> IParser ()
lastChar = modify' . set istLastChar
{-# INLINE lastChar #-}

-- | Lookup a link\/image reference definition.
lookupReference ::
  -- | Reference name
  Text ->
  -- | A collection of suggested reference names in 'Left' (typo
  -- corrections) or the requested definition in 'Right'
  IParser (Either [Text] (URI, Maybe Text))
lookupReference = lookupGeneric referenceDefs

-- | A generic function for looking up definition in 'IParser'.
lookupGeneric ::
  -- | How to access the definition map
  Lens' Defs (HashMap DefLabel a) ->
  -- | Definition name
  Text ->
  -- | A collection of suggested reference names in 'Left' (typo
  -- corrections) or the requested definition in 'Right'
  IParser (Either [Text] a)
lookupGeneric l name = do
  let dlabel = mkDefLabel name
  defs <- gets (view (istDefs . l))
  case HM.lookup dlabel defs of
    Nothing -> return . Left $ closeNames dlabel (HM.keys defs)
    Just x -> return (Right x)

-- | Select close enough (using the normalized Damerau-Levenshtein metric)
-- definition labels.
closeNames :: DefLabel -> [DefLabel] -> [Text]
closeNames r' =
  filter (\x -> damerauLevenshteinNorm r x >= (2 % 3))
    . map unDefLabel
  where
    r = unDefLabel r'

----------------------------------------------------------------------------
-- Helpers

-- | Setup an initial parser state.
mkInitialState ::
  -- | File name to use
  FilePath ->
  -- | Input
  Text ->
  -- | Starting offset
  Int ->
  M.State Text e
mkInitialState file input offset =
  M.State
    { stateInput = input,
      stateOffset = offset,
      statePosState =
        PosState
          { pstateInput = input,
            pstateOffset = offset,
            pstateSourcePos = initialPos file,
            pstateTabWidth = mkPos 4,
            pstateLinePrefix = ""
          },
      stateParseErrors = []
    }

-- | Locally change state in a state monad and then restore it back.
locally :: (MonadState s m) => Lens' s a -> a -> m b -> m b
locally l x m = do
  y <- gets (^. l)
  modify' (set l x)
  r <- m
  modify' (set l y)
  return r
