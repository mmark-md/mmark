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
import Data.Monoid ((<>))
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
  , try pFencedCodeBlock
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
  (ch, n, infoString) <- pOpeningCodeFence <* char '\n'
  -- FIXME what if the closing fence is placed in the middle of a line?
  ls <- manyTill (option "" grabLine <* char '\n') (eof <|> pClosingCodeFence ch n)
  CodeBlock infoString (assembleCodeBlock level ls) <$ sc

pOpeningCodeFence :: Parser (Char, Int, Maybe Text)
pOpeningCodeFence = p '`' <|> p '~'
  where
    p ch = try $ do
      void $ count 3 (char ch)
      n  <- (+ 3) . length <$> many (char ch)
      ml <- optional (T.strip <$> grabLine)
      return
        (ch, n,
           case ml of
             Nothing -> Nothing
             Just l  ->
               if T.null l
                 then Nothing
                 else Just l)

pClosingCodeFence :: Char -> Int -> Parser ()
pClosingCodeFence ch n = void . try $
  count n (char ch) <* many (char ch)

pIndentedCodeBlock :: Parser (Block Isp)
pIndentedCodeBlock = do
  void codeBlockLevel
  let go = do
        codeLevel <- lookAhead (True <$ codeBlockLevel <|> pure False)
        ml        <- lookAhead (optional grabLine)
        case (codeLevel, ml) of
          (False, Nothing) -> do
            continue <- grabNewline
            if continue then ("" :) <$> go else return []
          (False, Just _) ->
            return []
          (True, Nothing) -> do
            continue <- grabNewline
            if continue then ("" :) <$> go else return []
          (True, Just l) -> do
            void grabLine
            continue <- grabNewline
            (l :) <$> if continue then go else return []
  ls <- go
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
            if (isThematicBreak l || isHeading l) && spacePrefixLength l < 4
              then return []
              else do
                void grabLine
                continue <- grabNewline
                (T.strip l :) <$>
                  if continue then go else return []
  l        <- T.strip <$> grabLine
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
    stuff = NE.some . choice $ -- NOTE order matters here
      [ pCodeSpan ] <>
      [pInlineLink | allowLinks] <>
      [ pEnclosedInline
      , pPlain ]

pCodeSpan :: IParser Inline
pCodeSpan = do
  n <- try (length <$> some (char '`'))
  let finalizer = try $ do
        void $ count n (char '`')
        notFollowedBy (char '`')
  r <- CodeSpan . collapseWhiteSpace . T.concat <$>
    manyTill (takeWhile1P Nothing (== '`') <|> takeWhile1P Nothing (/= '`')) finalizer
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

pPlain :: IParser Inline
pPlain = Plain . T.pack <$> some (pEscapedChar <|> pNonEscapedChar)

pEscapedChar :: IParser Char
pEscapedChar = label "escaped character" $
  try (char '\\' *> pAsciiPunctuation <* put LastNonSpace)

pAsciiPunctuation :: IParser Char
pAsciiPunctuation = satisfy f
  where
    f x =
      (x >= '!' && x <= '/') ||
      (x >= ':' && x <= '@') ||
      (x >= '[' && x <= '`') ||
      (x >= '{' && x <= '~')

pNonEscapedChar :: IParser Char
pNonEscapedChar = label "unescaped non-markup character" $
  (spaceChar <* put LastSpace) <|>
  (satisfy (not . isMarkupChar) <* put LastNonSpace)

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

codeBlockLevel :: Parser Pos
codeBlockLevel = L.indentGuard sc GT (mkPos 4)

sc :: MonadParsec e Text m => m ()
sc = space

sc1 :: MonadParsec e Text m => m ()
sc1 = void $ takeWhile1P (Just "white space") isSpaceN

sc' :: Parser ()
sc' = void $ takeWhileP (Just "white space") spaceNoNewline

sc1' :: Parser ()
sc1' = void $ takeWhile1P (Just "white space") spaceNoNewline

spaceNoNewline :: Char -> Bool
spaceNoNewline x = x == '\t' || x == ' '

grabLine :: Parser Text
grabLine = takeWhile1P Nothing notNewline

notNewline :: Char -> Bool
notNewline x = x /= '\n' && x /= '\r'

nes :: a -> NonEmpty a
nes a = a :| []

isThematicBreak :: Text -> Bool
isThematicBreak l' = T.length l >= 3 &&
  (T.all (== '*') l ||
   T.all (== '-') l ||
   T.all (== '_') l)
  where
    l = T.filter (not . spaceNoNewline) l'

isHeading :: Text -> Bool
isHeading = isJust . parseMaybe p
  where
    p :: Parser ()
    p = count' 1 6 (char '#') *>
      (eof <|> eol <|> void (char ' ' <* takeRest))

eol :: (MonadParsec e s m, Token s ~ Char) => m ()
eol = void $ char '\n' <|> char '\r'

spacePrefixLength :: Text -> Int
spacePrefixLength = T.foldl' f 0 . T.takeWhile isSpace
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
    m = snd $ T.foldl' f (0, 0) (T.takeWhile isSpace txt)
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
    f EndOfInput = Label (NE.fromList "end of the inline block")
    f x          = x

-- mmarkErr :: MonadParsec MMarkErr s m => MMarkErr -> m a
-- mmarkErr = fancyFailure . E.singleton . ErrorCustom
