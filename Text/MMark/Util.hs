-- |
-- Module      :  Text.MMark.Util
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal utilities.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Util
  ( asPlainText
  , headerId
  , headerFragment )
where

import Data.Char (isSpace, isAlphaNum)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Text.MMark.Type
import Text.URI (URI (..))
import qualified Data.Text as T
import qualified Text.URI  as URI

-- | Convert a non-empty collection of 'Inline's into their plain text
-- representation. This is used e.g. to render image descriptions.

asPlainText :: NonEmpty Inline -> Text
asPlainText = foldMap $ \case
  Plain      txt -> txt
  LineBreak      -> "\n"
  Emphasis    xs -> asPlainText xs
  Strong      xs -> asPlainText xs
  Strikeout   xs -> asPlainText xs
  Subscript   xs -> asPlainText xs
  Superscript xs -> asPlainText xs
  CodeSpan   txt -> txt
  Link    xs _ _ -> asPlainText xs
  Image   xs _ _ -> asPlainText xs

-- | Generate value of id attribute for a given header. This is used during
-- rendering and also can be used to get id of a header for linking to it in
-- extensions.
--
-- See also: 'headerFragment'.

headerId :: NonEmpty Inline -> Text
headerId = T.intercalate "-"
  . T.words
  . T.filter (\x -> isAlphaNum x || isSpace x)
  . T.toLower
  . asPlainText

-- | Generate a 'URI' containing only a fragment from its textual
-- representation. Useful for getting URL from id of a header.

headerFragment :: Text -> URI
headerFragment fragment = URI
  { uriScheme    = Nothing
  , uriAuthority = Left False
  , uriPath      = []
  , uriQuery     = []
  , uriFragment  = URI.mkFragment fragment }
