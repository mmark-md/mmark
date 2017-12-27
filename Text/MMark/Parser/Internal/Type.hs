-- |
-- Module      :  Text.MMark.Parser.Internal.Type
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types for the internal helper definitions for the parser.

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Text.MMark.Parser.Internal.Type
  ( -- * Block-level parser state
    BlockState
  , bstAllowNaked
  , bstRefLevel
  , bstDefs
    -- * Inline-level parser state
  , InlineState
  , istLastChar
  , istAllowEmpty
  , istAllowLinks
  , istAllowImages
  , istDefs
  , Isp (..)
  , CharType (..)
    -- * Reference and footnote definitions
  , Defs
  , referenceDefs
  , DefLabel
  , mkDefLabel
  , unDefLabel
    -- * Other
  , MMarkErr (..) )
where

import Control.DeepSeq
import Data.CaseInsensitive (CI)
import Data.Data (Data)
import Data.Default.Class
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Lens.Micro.TH
import Text.Megaparsec
import Text.URI (URI)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict  as HM
import qualified Data.List.NonEmpty   as NE
import qualified Data.Text            as T

----------------------------------------------------------------------------
-- Block-level parser state

-- | Block-level parser state.

data BlockState = BlockState
  { _bstAllowNaked :: Bool
    -- ^ Should we consider a paragraph that does not end with a blank line
    -- 'Naked'? It does not make sense to do so for top-level document, but
    -- in lists, 'Naked' text is pretty common.
  , _bstRefLevel :: Pos
    -- ^ Current reference level: 1 column for top-level of document, column
    -- where content starts for block quotes and lists.
  , _bstDefs :: Defs
    -- ^ Reference and footnote definitions
  }

instance Default BlockState where
  def = BlockState
    { _bstAllowNaked = False
    , _bstRefLevel   = pos1
    , _bstDefs       = def
    }

----------------------------------------------------------------------------
-- Inline-level parser state

-- | Inline-level parser state.

data InlineState = InlineState
  { _istLastChar :: !CharType
    -- ^ Type of the last encountered character
  , _istAllowEmpty :: Bool
    -- ^ Whether to allow empty inlines
  , _istAllowLinks :: Bool
    -- ^ Whether to allow parsing of links
  , _istAllowImages :: Bool
    -- ^ Whether to allow parsing of images
  , _istDefs :: Defs
    -- ^ Reference link definitions
  }

instance Default InlineState where
  def = InlineState
    { _istLastChar    = SpaceChar
    , _istAllowEmpty  = True
    , _istAllowLinks  = True
    , _istAllowImages = True
    , _istDefs        = def
    }

-- | 'Inline' source pending parsing.

data Isp
  = IspSpan SourcePos Text
    -- ^ We have an inline source pending parsing
  | IspError (ParseError Char MMarkErr)
    -- ^ We should just return this parse error
  deriving (Eq, Show)

-- | Type of the last seen character.

data CharType
  = SpaceChar          -- ^ White space or a transparent character
  | OtherChar          -- ^ Other character
  deriving (Eq)

----------------------------------------------------------------------------
-- Reference and footnote definitions

-- | An opaque container for reference and footnote definitions.

newtype Defs = Defs
  { _referenceDefs :: HashMap DefLabel (URI, Maybe Text)
    -- ^ Reference definitions containing a 'URI' and optionally title
  }

instance Default Defs where
  def = Defs
    { _referenceDefs = HM.empty
    }

-- | An opaque type for definition label.

newtype DefLabel = DefLabel (CI Text)
  deriving (Eq, Ord, Hashable)

-- | Smart constructor for the 'DefLabel' type.

mkDefLabel :: Text -> DefLabel
mkDefLabel = DefLabel . CI.mk . T.unwords . T.words

-- | Extract 'Text' value from a 'DefLabel'.

unDefLabel :: DefLabel -> Text
unDefLabel (DefLabel x) = CI.original x

----------------------------------------------------------------------------
-- Other

-- | MMark custom parse errors.

data MMarkErr
  = YamlParseError String
    -- ^ YAML error that occurred during parsing of a YAML block
  | ListStartIndexTooBig Word
    -- ^ Ordered list start numbers must be nine digits or less
  | ListIndexOutOfOrder Word Word
    -- ^ The index in an ordered list is out of order, first number is the
    -- actual index we ran into, the second number is the expected index
  | NonFlankingDelimiterRun (NonEmpty Char)
    -- ^ This delimiter run should be in left- or right- flanking position
  | DuplicateReferenceDefinition Text
    -- ^ Duplicate reference definitions are not allowed
  | CouldNotFindReferenceDefinition Text [Text]
    -- ^ Could not find this reference definition, the second argument is
    -- the collection of close names (typo corrections)
  | InvalidNumericCharacter Int
    -- ^ This numeric character is invalid
  | UnknownHtmlEntityName Text
    -- ^ Unknown HTML5 entity name
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

instance ShowErrorComponent MMarkErr where
  showErrorComponent = \case
    YamlParseError str ->
      "YAML parse error: " ++ str
    ListStartIndexTooBig n ->
      "ordered list start numbers must be nine digits or less, " ++ show n
        ++ " is too big"
    ListIndexOutOfOrder actual expected ->
      "list index is out of order: " ++ show actual ++ ", expected "
        ++ show expected
    NonFlankingDelimiterRun dels ->
      showTokens dels ++ " should be in left- or right- flanking position"
    DuplicateReferenceDefinition name ->
      "duplicate reference definitions are not allowed: \""
        ++ T.unpack name ++ "\""
    CouldNotFindReferenceDefinition name alts ->
      "could not find a matching reference definition for \""
        ++ T.unpack name ++ "\""
        ++ case NE.nonEmpty alts of
             Nothing -> ""
             Just xs -> "\nperhaps you meant "
               ++ orList (quote . T.unpack <$> xs) ++ "?"
      where
        quote x = "\"" ++ x ++ "\""
    InvalidNumericCharacter n ->
      "invalid numeric character: " ++ show n
    UnknownHtmlEntityName name ->
      "unknown HTML5 entity name: \"" ++ T.unpack name ++ "\""

instance NFData MMarkErr

-- | Print a pretty list where items are separated with commas and the word
-- “or” according to the rules of English punctuation.

orList :: NonEmpty String -> String
orList (x:|[])  = x
orList (x:|[y]) = x <> " or " <> y
orList xs       = intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

----------------------------------------------------------------------------
-- Lens TH

makeLenses ''BlockState
makeLenses ''InlineState
makeLenses ''Defs
