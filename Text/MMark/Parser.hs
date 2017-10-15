-- |
-- Module      :  Text.MMark.Parser
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- MMark parser.

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Text.MMark.Parser
  ( MMarkErr (..)
  , parse )
where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Text.MMark.Internal
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char hiding (eol)
import qualified Control.Applicative.Combinators.NonEmpty as NE
import qualified Data.List.NonEmpty                       as NE
import qualified Data.Set                                 as E
import qualified Data.Text                                as T
import qualified Text.Megaparsec.Char.Lexer               as L

----------------------------------------------------------------------------
-- Data types

-- | Parser type we use internally.

type Parser = Parsec MMarkErr Text

-- | Parser type for inlines.

type IParser = StateT LastChar (Parsec MMarkErr Text)

-- | MMark custom parse errors.

data MMarkErr = MMarkDummy
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

instance ShowErrorComponent MMarkErr where
  showErrorComponent MMarkDummy = "mmark dummy error"

-- | Type of last parsed character: white space or not?

data LastChar = LastSpace | LastNonSpace
  deriving (Eq, Ord, Show)

-- | 'Inline' source pending parsing.

data Isp = Isp SourcePos Text
  deriving (Eq, Ord, Show)

-- | Frame that describes where we are in parsing inlines.

data InlineFrame
  = EmphasisFrame
  | EmphasisFrame_
  | StrongFrame
  | StrongFrame_
  | StrikeoutFrame
  | SubscriptFrame
  | SuperscriptFrame
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------------
-- Block parser

-- | Parse a markdown document in the form of a strict 'Text' value and
-- either report parse errors or return a 'MMark' document. Note that the
-- parser has the ability to report multiple parse errors at once.
--
-- __Pro tip__: use @'parseErrorPretty_' ('mkPos' 4)@ to pretty print parse
-- errors, because Common Mark suggests that we should assume tab width 4,
-- and that's what we do in the parser.

parse
  :: String
     -- ^ File name (only to be used in error messages), may be empty
  -> Text
     -- ^ Input to parse
  -> Either (NonEmpty (ParseError Char MMarkErr)) MMark
     -- ^ Parse errors or parsed document
parse file input =
  case runParser pBlocks file input of
    -- NOTE This parse error only happens when document structure on block
    -- level cannot be parsed, which should not normally happen.
    Left err -> Left (nes err)
    Right blocks ->
      let parsed = fmap (runIsp (pInlines True <* eof)) <$> blocks
          getErrs (Left e) es = replaceEof e : es
          getErrs _        es = es
          fromRight (Right x) = x
          fromRight _         =
            error "Text.MMark.Parser.parse: impossible happened"
      in case concatMap (foldr getErrs []) parsed of
           [] -> Right MMark
             { mmarkYaml      = Nothing
             , mmarkBlocks    = fmap fromRight <$> parsed
             , mmarkExtension = mempty }
           es -> (Left . NE.fromList . reverse) es

pBlocks :: Parser [Block Isp]
pBlocks = do
  setTabWidth (mkPos 4)
  sc *> manyTill pBlock eof

pBlock :: Parser (Block Isp)
pBlock = choice
  [ try pThematicBreak
  , try pAtxHeading
  , pFencedCodeBlock
  , try pIndentedCodeBlock
  , pParagraph ]

pThematicBreak :: Parser (Block Isp)
pThematicBreak = do
  void casualLevel
  l <- grabLine
  if isThematicBreak l
    then ThematicBreak <$ sc
    else empty

pAtxHeading :: Parser (Block Isp)
pAtxHeading = do
  void casualLevel
  hlevel <- length <$> some (char '#')
  guard (hlevel <= 6)
  finished <- (True <$ eof) <|> grabNewline
  (ispPos, heading) <-
    if finished
      then (,) <$> getPosition <*> pure ""
      else do
        sc1'
        ispPos <- getPosition
        let normalHeading = manyTill anyChar . try $
              optional (sc1' *> some (char '#') *> sc') *> (eof <|> eol)
            emptyHeading = "" <$
              optional (some (char '#') *> sc') <* (eof <|> eol)
        r <- try emptyHeading <|> normalHeading
        return (ispPos, T.pack r)
  let toBlock = case hlevel of
        1 -> Heading1
        2 -> Heading2
        3 -> Heading3
        4 -> Heading4
        5 -> Heading5
        _ -> Heading6
  toBlock (Isp ispPos (T.strip heading)) <$ sc

pFencedCodeBlock :: Parser (Block Isp)
pFencedCodeBlock = do
  level <- casualLevel
  let p ch = try $ do
        void $ count 3 (char ch)
        n  <- (+ 3) . length <$> many (char ch)
        ml <- optional (T.strip <$> grabLine <?> "info string")
        guard (maybe True (not . T.any (== '`')) ml)
        return
          (ch, n,
             case ml of
               Nothing -> Nothing
               Just l  ->
                 if T.null l
                   then Nothing
                   else Just l)
  (ch, n, infoString) <- (p '`' <|> p '~') <* eol
  let content = label "code block content" (option "" grabLine <* eol)
      closingFence = try . label "closing code fence" $ do
        void casualLevel'
        void $ count n (char ch)
        (void . many . char) ch
        sc'
        eof <|> eol
  ls <- manyTill content closingFence
  CodeBlock infoString (assembleCodeBlock level ls) <$ sc

pIndentedCodeBlock :: Parser (Block Isp)
pIndentedCodeBlock = do
  initialIndent <- codeBlockLevel
  let go ls = do
        immediate <- lookAhead (True <$ try codeBlockLevel' <|> pure False)
        eventual  <- lookAhead (True <$ try codeBlockLevel  <|> pure False)
        if not immediate && not eventual
          then return ls
          else do
            l        <- option "" grabLine
            continue <- grabNewline
            if continue
              then go (l:ls)
              else return (l:ls)
      -- NOTE This is a bit unfortunate, but it's difficult to guarantee
      -- that preceding space is not yet consumed when we get to
      -- interpreting input as an indented code block, so we need to restore
      -- the space this way.
      f x      = T.replicate (unPos initialIndent - 1) " " <> x
      g []     = []
      g (x:xs) = f x : xs
  ls <- g . reverse . dropWhile isBlankLine <$> go []
  CodeBlock Nothing (assembleCodeBlock (mkPos 5) ls) <$ sc

pParagraph :: Parser (Block Isp)
pParagraph = do
  void casualLevel
  startPos <- getPosition
  let go = do
        ml <- lookAhead (optional grabLine)
        case ml of
          Nothing -> return []
          Just l ->
            if or [ isThematicBreak l
                  , isHeading l
                  , isFencedCodeBlock l
                  , isBlankLine l ]
              then return []
              else do
                void grabLine
                continue <- grabNewline
                (l :) <$> if continue then go else return []
  l        <- grabLine
  continue <- grabNewline
  ls       <- if continue then go else return []
  Paragraph (Isp startPos (assembleParagraph (l:ls))) <$ sc

----------------------------------------------------------------------------
-- Inline parser

-- | Run given parser on 'Isp'.

runIsp
  :: IParser a
  -> Isp
  -> Either (ParseError Char MMarkErr) a
runIsp p (Isp startPos input) =
  snd (runParser' (evalStateT p LastSpace) pst)
  where
    pst = State
      { stateInput           = input
      , statePos             = nes startPos
      , stateTokensProcessed = 0
      , stateTabWidth        = mkPos 4 }

-- | Parse a stream of 'Inline's.

pInlines :: Bool -> IParser (NonEmpty Inline)
pInlines allowLinks = nes (Plain "") <$ eof <|> stuff
  where
    stuff = NE.some . label "inline content" . choice $
      [ pCodeSpan ] <>
      [pInlineLink | allowLinks] <>
      [ pEnclosedInline
      , try pHardLineBreak
      , pPlain ]

pCodeSpan :: IParser Inline
pCodeSpan = do
  n <- try (length <$> some (char '`'))
  let finalizer = try $ do
        void $ count n (char '`')
        notFollowedBy (char '`')
  r <- CodeSpan . collapseWhiteSpace . T.concat <$>
    manyTill (label "code span content" $
               takeWhile1P Nothing (== '`') <|>
               takeWhile1P Nothing (/= '`'))
      finalizer
  put LastNonSpace
  return r

pInlineLink :: IParser Inline
pInlineLink = do
  xs <- between (char '[') (char ']') (pInlines False)
  void (char '(') <* sc
  -- TODO use just normal escaping technique here, allow escape all ASCII
  -- punctuation as usual
  let enclosedLink = fmap T.pack . between (char '<') (char '>') . many $
        (try (char '\\' *> char '<') <?> "escaped '<'") <|>
        (try (char '\\' *> char '>') <?> "escaped '>'") <|>
        (satisfy (linkChar '<' '>')  <?> "unescaped link character")
      normalLink   = fmap T.pack . many $
        (try (char '\\' *> char '(') <?> "escaped '('") <|>
        (try (char '\\' *> char ')') <?> "escaped ')'") <|>
        (satisfy (linkChar '(' ')')  <?> "unescaped link character")
      linkChar x y ch = not (isSpaceN ch) && ch /= x && ch /= y
  dest <- enclosedLink <|> normalLink
  let enclosedWithEscape start end name =
        fmap T.pack . between (char start) (char end) . many $
          (try (char '\\' *> char end) <?> ("escaped " ++ name))
          <|> (satisfy (/= end) <?> "unescaped character")
  mtitle <- optional $ sc1 *> choice
    [ enclosedWithEscape '\"' '\"' "double quote"
    , enclosedWithEscape '\'' '\'' "single quote"
    , enclosedWithEscape '(' ')'   "parentheses" ]
  sc <* char ')'
  return (Link xs dest mtitle)

pEnclosedInline :: IParser Inline
pEnclosedInline = do
  -- TODO this approach does not allow to support *** stuff properly
  frame <- choice -- NOTE order matters here
    [ pLfdr StrongFrame
    , pLfdr EmphasisFrame
    , pLfdr StrongFrame_
    , pLfdr EmphasisFrame_
    , pLfdr StrikeoutFrame
    , pLfdr SubscriptFrame
    , pLfdr SuperscriptFrame ]
  xs <- pInlines True <* pRfdr frame
  return $ case frame of
    StrongFrame      -> Strong      xs
    EmphasisFrame    -> Emphasis    xs
    StrongFrame_     -> Strong      xs
    EmphasisFrame_   -> Emphasis    xs
    StrikeoutFrame   -> Strikeout   xs
    SubscriptFrame   -> Subscript   xs
    SuperscriptFrame -> Superscript xs

pLfdr :: InlineFrame -> IParser InlineFrame
pLfdr frame = try $ do
  mpos <- getNextTokenPosition
  (void . string . inlineFrameDel) frame
  lastSpace <- get
  case lastSpace of
    LastSpace -> return ()
    LastNonSpace -> do
      forM_ mpos setPosition
      -- FIXME improve errors later
      fail ("can't open " ++ inlineFramePretty frame ++ " here")
  notFollowedBy (space1 <|> eol) -- FIXME parse error sucks because of this
  return frame

pRfdr :: InlineFrame -> IParser ()
pRfdr frame = do
  mpos <- getNextTokenPosition
  (void . string . inlineFrameDel) frame
  lastSpace <- get
  case lastSpace of
    LastNonSpace -> return ()
    _ -> do
      forM_ mpos setPosition
      -- FIXME improve errors later
      fail ("can't close " ++ inlineFramePretty frame ++ " here")

pHardLineBreak :: IParser Inline
pHardLineBreak = do
  void (char '\\')
  eol
  notFollowedBy eof
  sc'
  put LastSpace
  return LineBreak

pPlain :: IParser Inline
pPlain = Plain . T.pack <$> some
  (pEscapedChar <|> pNewline <|> pNonEscapedChar)
  where
    pEscapedChar = label "escaped character" $
      try (char '\\' *> pAsciiPunctuation <* put LastNonSpace)
    pNewline = hidden . try $
      '\n' <$ sc' <* eol <* sc' <* put LastSpace
    pNonEscapedChar = label "unescaped non-markup character" . choice $
      [ try (char '\\' <* notFollowedBy eol <* put LastNonSpace)
      , spaceChar <* put LastSpace
      , satisfy f <* put LastNonSpace ]
    f x = not (isMarkupChar x) && x /= '\\'

pAsciiPunctuation :: IParser Char
pAsciiPunctuation = satisfy f
  where
    f x =
      (x >= '!' && x <= '/') ||
      (x >= ':' && x <= '@') ||
      (x >= '[' && x <= '`') ||
      (x >= '{' && x <= '~')

isMarkupChar :: Char -> Bool
isMarkupChar = \case
  '*' -> True
  '~' -> True
  '_' -> True
  '`' -> True
  '^' -> True
  '[' -> True
  ']' -> True
  _   -> False

----------------------------------------------------------------------------
-- Helpers

casualLevel :: Parser Pos
casualLevel = L.indentGuard sc LT (mkPos 5)

casualLevel' :: Parser Pos
casualLevel' = L.indentGuard sc' LT (mkPos 5)

codeBlockLevel :: Parser Pos
codeBlockLevel = L.indentGuard sc GT (mkPos 4)

codeBlockLevel' :: Parser Pos
codeBlockLevel' = L.indentGuard sc' GT (mkPos 4)

sc :: MonadParsec e Text m => m ()
sc = space

sc1 :: MonadParsec e Text m => m ()
sc1 = void $ takeWhile1P (Just "white space") isSpaceN

sc' :: MonadParsec e Text m => m ()
sc' = void $ takeWhileP (Just "white space") isSpaceNoNewline

sc1' :: MonadParsec e Text m => m ()
sc1' = void $ takeWhile1P (Just "white space") isSpaceNoNewline

isSpaceNoNewline :: Char -> Bool
isSpaceNoNewline x = x == '\t' || x == ' '

grabLine :: Parser Text
grabLine = takeWhile1P Nothing notNewline

notNewline :: Char -> Bool
notNewline x = x /= '\n' && x /= '\r'

isBlankLine :: Text -> Bool
isBlankLine = T.all isSpaceNoNewline

nes :: a -> NonEmpty a
nes a = a :| []

isThematicBreak :: Text -> Bool
isThematicBreak l' = T.length l >= 3 && indentLevel l' < 4 &&
  (T.all (== '*') l ||
   T.all (== '-') l ||
   T.all (== '_') l)
  where
    l = T.filter (not . isSpaceNoNewline) l'

isHeading :: Text -> Bool
isHeading = isJust . parseMaybe p . stripIndent (mkPos 4)
  where
    p :: Parser ()
    p = count' 1 6 (char '#') *>
      (eof <|> eol <|> void (char ' ' <* takeRest))

isFencedCodeBlock :: Text -> Bool
isFencedCodeBlock txt' = f '`' || f '~'
  where
    f ch = (T.replicate 3 (T.singleton ch) `T.isPrefixOf` txt) &&
      not (T.any (== ch) (T.dropWhile (== ch) txt))
    txt = stripIndent (mkPos 4) txt'

eol :: (MonadParsec e s m, Token s ~ Char) => m ()
eol = void (char '\n' <|> char '\r') <?> "newline"

indentLevel :: Text -> Int
indentLevel = T.foldl' f 0 . T.takeWhile isSpaceNoNewline
  where
    f n ch
      | ch == ' '  = n + 1
      | ch == '\t' = n + 4
      | otherwise  = n

grabNewline :: Parser Bool
grabNewline = choice
  [ True <$ char '\n'
  , True <$ char '\r'
  , pure False ]

assembleParagraph :: [Text] -> Text
assembleParagraph = T.intercalate "\n"

assembleCodeBlock :: Pos -> [Text] -> Text
assembleCodeBlock indent ls = T.unlines (stripIndent indent <$> ls)

stripIndent :: Pos -> Text -> Text
stripIndent indent txt = T.drop m txt
  where
    m = snd $ T.foldl' f (0, 0) (T.takeWhile isSpaceNoNewline txt)
    f (!j, !n) ch
      | j  >= i    = (j, n)
      | ch == ' '  = (j + 1, n + 1)
      | ch == '\t' = (j + 4, n + 1)
      | otherwise  = (j, n)
    i = unPos indent - 1

isSpace :: Char -> Bool
isSpace x = x == ' ' || x == '\t'

isSpaceN :: Char -> Bool
isSpaceN x = isSpace x || x == '\n' || x == '\r'

collapseWhiteSpace :: Text -> Text
collapseWhiteSpace =
  T.stripEnd . T.filter (/= '\0') . snd . T.mapAccumL f True
  where
    f seenSpace ch =
      case (seenSpace, g ch) of
        (False, False) -> (False, ch)
        (True,  False) -> (False, ch)
        (False, True)  -> (True,  ' ')
        (True,  True)  -> (True,  '\0')
    g ' '  = True
    g '\t' = True
    g '\n' = True
    g _    = False

inlineFrameDel :: InlineFrame -> Text
inlineFrameDel = \case
  EmphasisFrame    -> "*"
  EmphasisFrame_   -> "_"
  StrongFrame      -> "**"
  StrongFrame_     -> "__"
  StrikeoutFrame   -> "~~"
  SubscriptFrame   -> "~"
  SuperscriptFrame -> "^"

inlineFramePretty :: InlineFrame -> String
inlineFramePretty = \case
  EmphasisFrame    -> "*emphasis*"
  EmphasisFrame_   -> "_emphasis_"
  StrongFrame      -> "**strong emphasis**"
  StrongFrame_     -> "__strong emphasis__"
  StrikeoutFrame   -> "~~strikeout~~"
  SubscriptFrame   -> "~subscript~"
  SuperscriptFrame -> "^superscript^"

replaceEof :: ParseError Char e -> ParseError Char e
replaceEof = \case
  TrivialError pos us es -> TrivialError pos (f <$> us) (E.map f es)
  FancyError   pos xs    -> FancyError pos xs
  where
    f EndOfInput = Label (NE.fromList "end of inline block")
    f x          = x

-- mmarkErr :: MonadParsec MMarkErr s m => MMarkErr -> m a
-- mmarkErr = fancyFailure . E.singleton . ErrorCustom
