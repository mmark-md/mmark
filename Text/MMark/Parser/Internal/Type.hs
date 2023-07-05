{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Text.MMark.Parser.Internal.Type
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types for the internal helper definitions for the parser.
module Text.MMark.Parser.Internal.Type
  ( -- * Block-level parser state
    BlockState,
    initialBlockState,
    bstAllowNaked,
    bstRefLevel,
    bstDefs,

    -- * Inline-level parser state
    InlineState,
    initialInlineState,
    istLastChar,
    istAllowEmpty,
    istAllowLinks,
    istAllowImages,
    istDefs,
    Isp (..),
    CharType (..),

    -- * Reference and footnote definitions
    Defs,
    referenceDefs,
    DefLabel,
    mkDefLabel,
    unDefLabel,

    -- * Other
    MMarkErr (..),
  )
where

import Control.DeepSeq
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import GHC.Generics
import Lens.Micro.TH
import Text.Megaparsec
import Text.URI (URI)

----------------------------------------------------------------------------
-- Block-level parser state

-- | Block-level parser state.
data BlockState = BlockState
  { -- | Should we consider a paragraph that does not end with a blank line
    -- 'Naked'? It does not make sense to do so in the top-level document,
    -- but in lists, 'Naked' text is pretty common.
    _bstAllowNaked :: Bool,
    -- | Current reference level: 1 column for top-level of document, column
    -- where content starts for block quotes and lists.
    _bstRefLevel :: Pos,
    -- | Reference and footnote definitions
    _bstDefs :: Defs
  }

-- | Initial value for 'BlockState'.
initialBlockState :: BlockState
initialBlockState =
  BlockState
    { _bstAllowNaked = False,
      _bstRefLevel = pos1,
      _bstDefs = emptyDefs
    }

----------------------------------------------------------------------------
-- Inline-level parser state

-- | Inline-level parser state.
data InlineState = InlineState
  { -- | Type of the last encountered character
    _istLastChar :: !CharType,
    -- | Whether to allow empty inlines
    _istAllowEmpty :: Bool,
    -- | Whether to allow parsing of links
    _istAllowLinks :: Bool,
    -- | Whether to allow parsing of images
    _istAllowImages :: Bool,
    -- | Reference link definitions
    _istDefs :: Defs
  }

-- | Initial value for 'InlineState'.
initialInlineState :: InlineState
initialInlineState =
  InlineState
    { _istLastChar = SpaceChar,
      _istAllowEmpty = True,
      _istAllowLinks = True,
      _istAllowImages = True,
      _istDefs = emptyDefs
    }

-- | 'Inline' source pending parsing.
data Isp
  = -- | We have an inline source pending parsing
    IspSpan Int Text
  | -- | We should just return this parse error
    IspError (ParseError Text MMarkErr)
  deriving (Eq, Show)

-- | Type of the last seen character.
data CharType
  = -- | White space or a transparent character
    SpaceChar
  | -- | Punctuation character
    PunctChar
  | -- | Other character
    OtherChar
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------------
-- Reference and footnote definitions

-- | An opaque container for reference and footnote definitions.
newtype Defs = Defs
  { -- | Reference definitions containing a 'URI' and optionally title
    _referenceDefs :: HashMap DefLabel (URI, Maybe Text)
  }

-- | Empty 'Defs'.
emptyDefs :: Defs
emptyDefs =
  Defs
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
  = -- | YAML error that occurred during parsing of a YAML block
    YamlParseError String
  | -- | This delimiter run should be in left- or right- flanking position
    NonFlankingDelimiterRun (NonEmpty Char)
  | -- | Ordered list start numbers must be nine digits or less
    --
    -- @since 0.0.2.0
    ListStartIndexTooBig Word
  | -- | The index in an ordered list is out of order, first number is the
    -- actual index we ran into, the second number is the expected index
    --
    -- @since 0.0.2.0
    ListIndexOutOfOrder Word Word
  | -- | Duplicate reference definitions are not allowed
    --
    -- @since 0.0.3.0
    DuplicateReferenceDefinition Text
  | -- | Could not find this reference definition, the second argument is
    -- the collection of close names (typo corrections)
    --
    -- @since 0.0.3.0
    CouldNotFindReferenceDefinition Text [Text]
  | -- | This numeric character is invalid
    --
    -- @since 0.0.3.0
    InvalidNumericCharacter Int
  | -- | Unknown HTML5 entity name
    --
    -- @since 0.0.3.0
    UnknownHtmlEntityName Text
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

instance ShowErrorComponent MMarkErr where
  showErrorComponent = \case
    YamlParseError str ->
      "YAML parse error: " ++ str
    NonFlankingDelimiterRun dels ->
      showTokens (Proxy :: Proxy Text) dels
        ++ " should be in left- or right- flanking position"
    ListStartIndexTooBig n ->
      "ordered list start numbers must be nine digits or less, "
        ++ show n
        ++ " is too big"
    ListIndexOutOfOrder actual expected ->
      "list index is out of order: "
        ++ show actual
        ++ ", expected "
        ++ show expected
    DuplicateReferenceDefinition name ->
      "duplicate reference definitions are not allowed: \""
        ++ T.unpack name
        ++ "\""
    CouldNotFindReferenceDefinition name alts ->
      "could not find a matching reference definition for \""
        ++ T.unpack name
        ++ "\""
        ++ case NE.nonEmpty alts of
          Nothing -> ""
          Just xs ->
            "\nperhaps you meant "
              ++ orList (quote . T.unpack <$> xs)
              ++ "?"
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
orList (x :| []) = x
orList (x :| [y]) = x <> " or " <> y
orList xs = intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

----------------------------------------------------------------------------
-- Lens TH

makeLenses ''BlockState
makeLenses ''InlineState
makeLenses ''Defs
