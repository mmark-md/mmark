-- |
-- Module      :  Text.MMark.Parser.Internal
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- An internal module that builds a framework on which the
-- "Text.MMark.Parser" module is built.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Text.MMark.Parser.Internal
  ( -- * Block-level parser monad
    BParser
  , runBParser
  , isNakedAllowed
  , refLevel
  , subEnv
  , registerReference
  , registerFootnote
    -- * Inline-level parser monad
  , IParser
  , runIParser
  , disallowEmpty
  , isEmptyAllowed
  , disallowLinks
  , isLinksAllowed
  , disallowImages
  , isImagesAllowed
  , isLastSpace
  , isLastOther
  , lastSpace
  , lastOther
  , lookupReference
  , lookupFootnote
  , Isp (..)
    -- * Reference and footnote definitions
  , Defs
    -- * Other
  , MMarkErr (..) )
where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Default.Class
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text.Metrics (damerauLevenshteinNorm)
import Lens.Micro (Lens', (^.), (.~), set, over, to)
import Lens.Micro.Extras (view)
import Text.MMark.Internal
import Text.MMark.Parser.Internal.Type
import Text.Megaparsec hiding (State)
import Text.URI (URI)
import qualified Data.HashMap.Strict as HM
import qualified Text.Megaparsec     as M

----------------------------------------------------------------------------
-- Block-level parser monad

-- | Block-level parser type.

newtype BParser a = BParser
  { unBParser :: ParsecT MMarkErr Text (State BlockState) a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , MonadParsec MMarkErr Text )

-- | Run a computation in the 'BParser' monad.

runBParser
  :: BParser a
     -- ^ The parser to run
  -> FilePath
     -- ^ File name (only to be used in error messages), may be empty
  -> Text
     -- ^ Input to parse
  -> Either (NonEmpty (ParseError Char MMarkErr)) (a, Defs)
     -- ^ Result of parsing
runBParser (BParser p) file input =
  case runState (runParserT p file input) def of
    (Left err, _) -> Left  (err :| [])
    (Right x, st) -> Right (x, st ^. bstDefs)

-- | Ask whether naked paragraphs are allowed in this context.

isNakedAllowed :: BParser Bool
isNakedAllowed = BParser $ gets (^. bstAllowNaked)

-- | Lookup current reference indentation level.

refLevel :: BParser Pos
refLevel = BParser $ gets (^. bstRefLevel)

-- | Execute 'BParser' computation with modified environment.

subEnv
  :: Bool              -- ^ Whether naked paragraphs should be allowed
  -> Pos               -- ^ Reference indentation level
  -> BParser a         -- ^ The parser we want to set the environment for
  -> BParser a         -- ^ The resulting parser
subEnv allowNaked rlevel
  = BParser
  . locally bstAllowNaked allowNaked
  . locally bstRefLevel   rlevel
  . unBParser

-- | Register a reference (link\/image) definition.

registerReference
  :: Text              -- ^ Reference name
  -> (URI, Maybe Text) -- ^ Reference 'URI' and optional title
  -> BParser Bool      -- ^ 'True' if there is a conflicting definition
registerReference = registerGeneric referenceDefs

-- | Register a footnote definition.

registerFootnote
  :: Text              -- ^ Reference name
  -> NonEmpty Inline   -- ^ FIXME Footnote definition
  -> BParser Bool      -- ^ 'True' if there is a conflicting definition
registerFootnote = registerGeneric footnoteDefs

-- | A generic function for registering definitions in 'BParser'.

registerGeneric
  :: Lens' Defs (HashMap DefLabel a) -- ^ How to access the definition map
  -> Text              -- ^ Definition name
  -> a                 -- ^ Data
  -> BParser Bool      -- ^ 'True' if there is a conflicting definition
registerGeneric l name a = BParser $ do
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

newtype IParser a = IParser
  { unIParser :: StateT InlineState (Parsec MMarkErr Text) a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , MonadParsec MMarkErr Text )

-- | Run a computation in the 'IParser' monad.

runIParser
  :: Defs
     -- ^ Reference and footnote definitions obtained as a result of
     -- block-level parsing
  -> IParser a
     -- ^ The parser to run
  -> Isp
     -- ^ Input for the parser
  -> Either (ParseError Char MMarkErr) a
     -- ^ Result of parsing
runIParser _ _ (IspError err) = Left err
runIParser defs (IParser p) (IspSpan startPos input) =
  snd (runParser' (evalStateT p ist) pst)
  where
    ist = def & istDefs .~ defs
    pst = M.State
      { stateInput           = input
      , statePos             = startPos :| []
      , stateTokensProcessed = 0
      , stateTabWidth        = mkPos 4
      }

-- | Disallow parsing of empty inlines.

disallowEmpty :: IParser a -> IParser a
disallowEmpty = IParser . locally istAllowEmpty False . unIParser

-- | Ask whether parsing of empty inlines is allowed.

isEmptyAllowed :: IParser Bool
isEmptyAllowed = IParser . gets $ view istAllowEmpty

-- | Disallow parsing of links.

disallowLinks :: IParser a -> IParser a
disallowLinks = IParser . locally istAllowLinks False . unIParser

-- | Ask whether parsing of links is allowed.

isLinksAllowed :: IParser Bool
isLinksAllowed = IParser . gets $ view istAllowLinks

-- | Disallow parsing of images.

disallowImages :: IParser a -> IParser a
disallowImages = IParser . locally istAllowImages False . unIParser

-- | Ask whether parsing of images is allowed.

isImagesAllowed :: IParser Bool
isImagesAllowed = IParser . gets $ view istAllowImages

-- | Ask whether the last seen char type was space.

isLastSpace :: IParser Bool
isLastSpace = IParser . gets $ view (istLastChar . to (== SpaceChar))

-- | Ask whether the last seen char type was “other” (not space).

isLastOther :: IParser Bool
isLastOther = IParser . gets $ view (istLastChar . to (== OtherChar))

-- | Register that the last seen char type is space.

lastSpace :: IParser ()
lastSpace = IParser . modify' $ set istLastChar SpaceChar
{-# INLINE lastSpace #-}

-- | Register that the last seen char type is “other” (not space).

lastOther :: IParser ()
lastOther = IParser . modify' $ set istLastChar OtherChar
{-# INLINE lastOther #-}

-- | Lookup a link\/image reference definition.

lookupReference
  :: Text
     -- ^ Reference name
  -> IParser (Either [Text] (URI, Maybe Text))
     -- ^ A collection of suggested reference names in 'Left' (typo
     -- corrections) or the requested definition in 'Right'
lookupReference = lookupGeneric referenceDefs

-- | Lookup a footnote definition.

lookupFootnote
  :: Text
     -- ^ Footnote name
  -> IParser (Either [Text] (NonEmpty Inline))
     -- ^ A collection of suggested reference names in 'Left' (typo
     -- corrections) or the requested definition in 'Right'
lookupFootnote = lookupGeneric footnoteDefs

-- | A generic function for looking up definition in 'IParser'.

lookupGeneric
  :: Lens' Defs (HashMap DefLabel a)
     -- ^ How to access the definition map
  -> Text
     -- ^ Definition name
  -> IParser (Either [Text] a)
     -- ^ A collection of suggested reference names in 'Left' (typo
     -- corrections) or the requested definition in 'Right'
lookupGeneric l name = IParser $ do
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

-- | Locally change state in a state monad and then restore it back.

locally :: MonadState s m => Lens' s a -> a -> m b -> m b
locally l x m = do
  y <- gets (^. l)
  modify' (set l x)
  r <- m
  modify' (set l y)
  return r
