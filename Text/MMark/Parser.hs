-- |
-- Module      :  Text.MMark.Parser
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- MMark parser. You probably want to import "Text.MMark" instead.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Text.MMark.Parser
  ( parseMMark )
where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Void
import Text.MMark.Internal
import Text.Megaparsec
import Text.Megaparsec.Char hiding (eol)
import qualified Control.Applicative.Combinators.NonEmpty as NE
import qualified Data.List.NonEmpty                       as NE
import qualified Data.Set                                 as E
import qualified Data.Text                                as T
import qualified Text.Megaparsec.Char.Lexer               as L

-- | Parser type we use internally.

type Parser = Parsec Void Text

-- | Parser type for inlines.

type IParser = StateT LastChar (Parsec Void Text)

data LastChar = LastSpace | LastNonSpace

-- | 'Inline' source pending parsing, result of initial run of the parser
-- which creates shape of the document in terms of 'Block's.

data Isp = Isp SourcePos Text
  deriving (Show)

-- | Frame that describes where we are in parsing inlines.

data InlineFrame
  = EmphasisFrame
  | EmphasisFrame_
  | StrongFrame
  | StrongFrame_
  | StrikeoutFrame
  | SuperscriptFrame
  deriving (Eq, Ord, Show)

-- | Parse a markdown document in the form of a strict 'Text' value and
-- either report parse errors or return a 'MMark' document. The parser is an
-- efficient parallel parser (meaning it can actually divide the work
-- between several threads) with the ability to report multiple parse
-- errors.

parseMMark
  :: String
     -- ^ File name (only to be used in error messages), may be empty
  -> Text
     -- ^ Input to parse
  -> Either (NonEmpty (ParseError Char Void)) MMark
     -- ^ Parse errors or resulting document
parseMMark file input =
  case parse pBlocks file input of
    Left err -> Left (nes err)
    Right blocks ->
      let parsed = fmap (runIsp (pInlines <* eof)) <$> blocks
          getErrs (Left e) es = replaceEof e : es
          getErrs _        es = es
          fromRight (Right x) = x
          fromRight _         = error "Oops!" -- should not happen
      in case concatMap (foldr getErrs []) parsed of
           [] -> Right MMark
             { mmarkYaml_     = Nothing
             , mmarkBlocks    = fmap fromRight <$> parsed
             , mmarkExtension = mempty }
           es -> (Left . NE.fromList . reverse) es

pBlocks :: Parser [Block Isp] -- TODO use withRecovery here if possible?
pBlocks = do
  setTabWidth (mkPos 4)
  between sc eof (many pBlock)

pBlock :: Parser (Block Isp)
pBlock = choice
  [ pThematicBreak
  , pAtxHeading
  , pFencedCodeBlock
  , pParagraph
  , pIndentedCodeBlock ]

pThematicBreak :: Parser (Block Isp)
pThematicBreak = try $ do
  void casualLevel
  l <- grabLine
  if isThematicBreak l
    then ThematicBreak <$ sc
    else empty

pAtxHeading :: Parser (Block Isp)
pAtxHeading = try $ do
  void casualLevel
  startPos <- getPosition
  hlevel   <- atxOpening
  let toBlock = case hlevel of
        1 -> Heading1
        2 -> Heading2
        3 -> Heading3
        4 -> Heading4
        5 -> Heading5
        _ -> Heading6
  finished <- grabNewline
  heading <-
    if finished
      then return ""
      else do
        void (char ' ')
        let justClosing = "" <$ some (char '#') <* sc' <* eol
            normHeading = T.pack <$> manyTill anyChar
              (try $ optional (try $ char ' ' *> some (char '#') *> sc') *> eol)
        try justClosing <|> normHeading
  toBlock (Isp startPos (T.strip heading)) <$ sc

pParagraph :: Parser (Block Isp)
pParagraph = try $ do
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

pFencedCodeBlock :: Parser (Block Isp)
pFencedCodeBlock = try $ do
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
pIndentedCodeBlock = try $ do
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

-- | Run given parser on 'Isp'.

runIsp
  :: IParser a
  -> Isp
  -> Either (ParseError Char Void) a
runIsp p (Isp startPos input) =
  snd (runParser' (evalStateT p LastSpace) pst)
  where
    pst = State
      { stateInput           = input
      , statePos             = nes startPos
      , stateTokensProcessed = 0
      , stateTabWidth        = mkPos 4 }

-- | Parse a stream of 'Inline's.

pInlines :: IParser (NonEmpty Inline)
pInlines = nes (Plain "") <$ eof <|> stuff
  where
    stuff = NE.some $ choice
      [ pCodeSpan -- NOTE order matters here
      , pEnclosedInline
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

pEnclosedInline :: IParser Inline
pEnclosedInline = do
  -- TODO this approach does not allow to support *** stuff properly
  frame <- choice -- NOTE order matters here
    [ pLfdr StrongFrame
    , pLfdr EmphasisFrame
    , pLfdr StrongFrame_
    , pLfdr EmphasisFrame_
    , pLfdr StrikeoutFrame
    , pLfdr SuperscriptFrame ]
  xs <- pInlines <* pRfdr frame
  return $ case frame of
    StrongFrame      -> Strong      xs
    EmphasisFrame    -> Emphasis    xs
    StrongFrame_     -> Strong      xs
    EmphasisFrame_   -> Emphasis    xs
    StrikeoutFrame   -> Strikeout   xs
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
  _   -> False

----------------------------------------------------------------------------
-- Helpers

casualLevel :: Parser Pos
casualLevel = L.indentGuard sc LT (mkPos 5)

codeBlockLevel :: Parser Pos
codeBlockLevel = L.indentGuard sc GT (mkPos 4)

sc :: Parser ()
sc = space

sc' :: Parser ()
sc' = void $ takeWhileP Nothing spaceNoNewline

-- sc1' :: Parser ()
-- sc1' = void $ takeWhile1P (Just "white space") spaceNoNewline

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
    l = T.filter (/= ' ') l'

atxOpening :: Parser Int
atxOpening = length <$> count' 1 6 (char '#')

isHeading :: Text -> Bool
isHeading = isJust . parseMaybe p
  where
    p :: Parser ()
    p = atxOpening *> (eof <|> eol <|> void (char ' ' <* takeRest))

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
    m = T.foldl' f 0 (T.takeWhile isSpace txt)
    f n ch
      | n >= i     = n
      | ch == ' '  = n + 1
      | ch == '\t' = n + 4
      | otherwise  = n
    i = unPos indent - 1

isSpace :: Char -> Bool
isSpace x = x == ' ' || x == '\t'

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
  SuperscriptFrame -> "^"

inlineFramePretty :: InlineFrame -> String
inlineFramePretty = \case
  EmphasisFrame    -> "*emphasis*"
  EmphasisFrame_   -> "_emphasis_"
  StrongFrame      -> "**strong emphasis**"
  StrongFrame_     -> "__strong emphasis__"
  StrikeoutFrame   -> "~~strikeout~~"
  SuperscriptFrame -> "^superscript^"

replaceEof :: ParseError Char Void -> ParseError Char Void
replaceEof = \case
  TrivialError pos us es -> TrivialError pos (f <$> us) (E.map f es)
  FancyError   pos xs    -> FancyError pos xs
  where
    f EndOfInput = Label (NE.fromList "end of the inline block")
    f x          = x
