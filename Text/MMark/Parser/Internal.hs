-- |
-- Module      :  Text.MMark.Parser.Internal
-- Copyright   :  © 2017–2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- An internal module that builds a framework on which the
-- "Text.MMark.Parser" module is built.

{-# LANGUAGE RankNTypes #-}

module Text.MMark.Parser.Internal
  ( -- * Block-level parser monad
    BParser
  , runBParser
  , isNakedAllowed
  , refLevel
  , subEnv
  , registerReference
    -- * Inline-level parser monad
  , IParser
  , runIParser
  , disallowEmpty
  , isEmptyAllowed
  , disallowLinks
  , isLinksAllowed
  , disallowImages
  , isImagesAllowed
  , getLastChar
  , lastChar
  , lookupReference
  , Isp (..)
  , CharType (..)
    -- * Reference and footnote definitions
  , Defs
    -- * Other
  , MMarkErr (..) )
where

import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text.Metrics (damerauLevenshteinNorm)
import Lens.Micro (Lens', (^.), (.~), set, over)
import Lens.Micro.Extras (view)
import Text.MMark.Parser.Internal.Type
import Text.Megaparsec hiding (State)
import Text.URI (URI)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NE
import qualified Text.Megaparsec     as M

----------------------------------------------------------------------------
-- Block-level parser monad

-- | Block-level parser type.

type BParser a = ParsecT MMarkErr Text (State BlockState) a

-- | Run a computation in the 'BParser' monad.

runBParser
  :: BParser a
     -- ^ The parser to run
  -> FilePath
     -- ^ File name (only to be used in error messages), may be empty
  -> Text
     -- ^ Input to parse
  -> Either (ParseErrorBundle Text MMarkErr) (a, Defs)
     -- ^ Result of parsing
runBParser p file input =
  case runState (snd <$> runParserT' p st) initialBlockState of
    (Left  bundle, _) -> Left bundle
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

subEnv
  :: Bool              -- ^ Whether naked paragraphs should be allowed
  -> Pos               -- ^ Reference indentation level
  -> BParser a         -- ^ The parser we want to set the environment for
  -> BParser a         -- ^ The resulting parser
subEnv allowNaked rlevel =
  locally bstAllowNaked allowNaked .
  locally bstRefLevel   rlevel

-- | Register a reference (link\/image) definition.

registerReference
  :: Text              -- ^ Reference name
  -> (URI, Maybe Text) -- ^ Reference 'URI' and optional title
  -> BParser Bool      -- ^ 'True' if there is a conflicting definition
registerReference = registerGeneric referenceDefs

-- | A generic function for registering definitions in 'BParser'.

registerGeneric
  :: Lens' Defs (HashMap DefLabel a) -- ^ How to access the definition map
  -> Text              -- ^ Definition name
  -> a                 -- ^ Data
  -> BParser Bool      -- ^ 'True' if there is a conflicting definition
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

runIParser
  :: Defs
     -- ^ Reference and footnote definitions obtained as a result of
     -- block-level parsing
  -> IParser a
     -- ^ The parser to run
  -> Isp
     -- ^ Input for the parser
  -> Either (ParseError Text MMarkErr) a
     -- ^ Result of parsing
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

lookupReference
  :: Text
     -- ^ Reference name
  -> IParser (Either [Text] (URI, Maybe Text))
     -- ^ A collection of suggested reference names in 'Left' (typo
     -- corrections) or the requested definition in 'Right'
lookupReference = lookupGeneric referenceDefs

-- | A generic function for looking up definition in 'IParser'.

lookupGeneric
  :: Lens' Defs (HashMap DefLabel a)
     -- ^ How to access the definition map
  -> Text
     -- ^ Definition name
  -> IParser (Either [Text] a)
     -- ^ A collection of suggested reference names in 'Left' (typo
     -- corrections) or the requested definition in 'Right'
lookupGeneric l name = do
  let dlabel = mkDefLabel name
  defs <- gets (view (istDefs . l))
  case HM.lookup dlabel defs of
    Nothing -> return . Left $ closeNames dlabel (HM.keys defs)
    Just  x -> return (Right x)

-- | Select close enough (using the normalized Damerau-Levenshtein metric)
-- definition labels.

closeNames :: DefLabel -> [DefLabel] -> [Text]
closeNames r'
  = filter (\x -> damerauLevenshteinNorm r x >= (2 % 3))
  . map unDefLabel
  where
    r = unDefLabel r'

----------------------------------------------------------------------------
-- Helpers

-- | Setup initial parser state.

mkInitialState
  :: FilePath          -- ^ File name to use
  -> Text              -- ^ Input
  -> Int               -- ^ Starting offset
  -> M.State Text
mkInitialState file input offset = M.State
  { stateInput = input
  , stateOffset = offset
  , statePosState = PosState
    { pstateInput = input
    , pstateOffset = offset
    , pstateSourcePos = initialPos file
    , pstateTabWidth = mkPos 4
    , pstateLinePrefix = ""
    }
  }

-- | Locally change state in a state monad and then restore it back.

locally :: MonadState s m => Lens' s a -> a -> m b -> m b
locally l x m = do
  y <- gets (^. l)
  modify' (set l x)
  r <- m
  modify' (set l y)
  return r
