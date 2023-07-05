{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Util
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal utilities.
module Text.MMark.Util
  ( asPlainText,
    headerId,
    headerFragment,
  )
where

import Data.Char (isAlphaNum, isSpace)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Text.MMark.Type
import Text.URI (URI (..))
import Text.URI qualified as URI

-- | Convert a non-empty collection of 'Inline's into their plain text
-- representation. This is used e.g. to render image descriptions.
asPlainText :: NonEmpty Inline -> Text
asPlainText = foldMap $ \case
  Plain txt -> txt
  LineBreak -> "\n"
  Emphasis xs -> asPlainText xs
  Strong xs -> asPlainText xs
  Strikeout xs -> asPlainText xs
  Subscript xs -> asPlainText xs
  Superscript xs -> asPlainText xs
  CodeSpan txt -> txt
  Link xs _ _ -> asPlainText xs
  Image xs _ _ -> asPlainText xs

-- | Generate value of id attribute for a given header. This is used during
-- rendering and also can be used to get id of a header for linking to it in
-- extensions.
--
-- See also: 'headerFragment'.
headerId :: NonEmpty Inline -> Text
headerId =
  T.intercalate "-"
    . T.words
    . T.filter (\x -> isAlphaNum x || isSpace x)
    . T.toLower
    . asPlainText

-- | Generate a 'URI' containing only a fragment from its textual
-- representation. Useful for getting URL from id of a header.
headerFragment :: Text -> URI
headerFragment fragment =
  URI
    { uriScheme = Nothing,
      uriAuthority = Left False,
      uriPath = Nothing,
      uriQuery = [],
      uriFragment = URI.mkFragment fragment
    }
