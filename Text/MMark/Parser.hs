-- |
-- Module      :  Text.MMark.Parser
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- MMark markdown parser.

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Text.MMark.Parser
  ( MMarkErr (..)
  , parse )
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Data)
import Data.Default.Class
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.Monoid (Any (..))
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void
import GHC.Generics
import Text.MMark.Internal
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char hiding (eol)
import Text.URI (URI)
import qualified Control.Applicative.Combinators.NonEmpty as NE
import qualified Data.Char                  as Char
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as E
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Yaml                  as Yaml
import qualified Text.Email.Validate        as Email
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.URI                   as URI

----------------------------------------------------------------------------
-- Data types

-- | Block-level parser type.

type BParser = ParsecT MMarkErr Text (Reader BlockEnv)

-- | Block-level parser environment.

data BlockEnv = BlockEnv
  { benvAllowNaked :: !Bool
    -- ^ Should we consider a paragraph that does not end with a blank line
    -- 'Naked'? It does not make sense to do so for top-level document, but
    -- in lists, 'Naked' text is pretty common.
  , benvRefLevel :: !Pos
    -- ^ Current reference level: 1 column for top-level of document, column
    -- where content starts for block quotes and lists.
  }

instance Default BlockEnv where
  def = BlockEnv
    { benvAllowNaked = False
    , benvRefLevel   = pos1
    }

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
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

instance ShowErrorComponent MMarkErr where
  showErrorComponent = \case
    YamlParseError str ->
      "YAML parse error: " ++ str
    ListStartIndexTooBig n ->
      "Ordered list start numbers must be nine digits or less, " ++ show n
        ++ " is too big"
    ListIndexOutOfOrder actual expected ->
      "List index out of order: " ++ show actual ++ ", expected "
        ++ show expected
    NonFlankingDelimiterRun dels ->
      showTokens dels ++ " should be in left- or right- flanking position"

instance NFData MMarkErr

-- | Inline-level parser type. We store type of the last consumed character
-- in the state.

type IParser = StateT CharType (Parsec MMarkErr Text)

-- | 'Inline' source pending parsing.

data Isp
  = IspSpan SourcePos Text
    -- ^ We have an inline source pending parsing
  | IspError (ParseError Char MMarkErr)
    -- ^ We should just return this parse error
  deriving (Eq, Show)

-- | Type of last seen character.

data CharType
  = SpaceChar          -- ^ White space or a transparent character
  | LeftFlankingDel    -- ^ Left flanking delimiter
  | RightFlankingDel   -- ^ Right flaking delimiter
  | OtherChar          -- ^ Other character
  deriving (Eq, Ord, Show)

-- | Frame that describes where we are in parsing inlines.

data InlineFrame
  = EmphasisFrame      -- ^ Emphasis with asterisk @*@
  | EmphasisFrame_     -- ^ Emphasis with underscore @_@
  | StrongFrame        -- ^ Strong emphasis with asterisk @**@
  | StrongFrame_       -- ^ Strong emphasis with underscore @__@
  | StrikeoutFrame     -- ^ Strikeout
  | SubscriptFrame     -- ^ Subscript
  | SuperscriptFrame   -- ^ Superscript
  deriving (Eq, Ord, Show)

-- | State of inline parsing that specifies whether we expect to close one
-- frame or there is a possibility to close one of two alternatives.

data InlineState
  = SingleFrame InlineFrame             -- ^ One frame to be closed
  | DoubleFrame InlineFrame InlineFrame -- ^ Two frames to be closed
  deriving (Eq, Ord, Show)

-- | Configuration of inline parser.

data InlineConfig = InlineConfig
  { iconfigAllowEmpty :: !Bool
    -- ^ Whether to accept empty inline blocks
  , iconfigAllowLinks :: !Bool
    -- ^ Whether to parse links
  , iconfigAllowImages :: !Bool
    -- ^ Whether to parse images
  }

instance Default InlineConfig where
  def = InlineConfig
    { iconfigAllowEmpty  = True
    , iconfigAllowLinks  = True
    , iconfigAllowImages = True
    }

-- | An auxiliary type for collapsing levels of 'Either's.

data Pair s a
  = PairL s
  | PairR ([a] -> [a])

instance Semigroup s => Semigroup (Pair s a) where
  (PairL l) <> (PairL r) = PairL (l <> r)
  (PairL l) <> (PairR _) = PairL l
  (PairR _) <> (PairL r) = PairL r
  (PairR l) <> (PairR r) = PairR (l . r)

instance Semigroup s => Monoid (Pair s a) where
  mempty  = PairR id
  mappend = (<>)

----------------------------------------------------------------------------
-- Top-level API

-- | Parse a markdown document in the form of a strict 'Text' value and
-- either report parse errors or return an 'MMark' document. Note that the
-- parser has the ability to report multiple parse errors at once.

parse
  :: String
     -- ^ File name (only to be used in error messages), may be empty
  -> Text
     -- ^ Input to parse
  -> Either (NonEmpty (ParseError Char MMarkErr)) MMark
     -- ^ Parse errors or parsed document
parse file input =
  case runReader (runParserT pMMark file input) def of
    -- NOTE This parse error only happens when document structure on block
    -- level cannot be parsed even with recovery, which should not normally
    -- happen except for the cases when we deal with YAML parsing errors.
    Left err -> Left (nes err)
    Right (myaml, rawBlocks) ->
      let parsed = doInline <$> rawBlocks
          doInline = fmap
            $ first (nes . replaceEof "end of inline block")
            . runIsp (pInlines def <* eof)
          f block =
            case foldMap e2p block of
              PairL errs -> PairL errs
              PairR _    -> PairR (fmap fromRight block :)
      in case foldMap f parsed of
           PairL errs   -> Left errs
           PairR blocks -> Right MMark
             { mmarkYaml      = myaml
             , mmarkBlocks    = blocks []
             , mmarkExtension = mempty }

----------------------------------------------------------------------------
-- Block parser

-- | Parse an MMark document on block level.

pMMark :: BParser (Maybe Yaml.Value, [Block Isp])
pMMark = do
  meyaml <- optional pYamlBlock
  setTabWidth (mkPos 4)
  blocks <- pBlocks
  eof
  return $ case meyaml of
    Nothing ->
      (Nothing, blocks)
    Just (Left (pos, err)) ->
      (Nothing, prependErr pos (YamlParseError err) blocks)
    Just (Right yaml) ->
      (Just yaml, blocks)

-- | Parse a YAML block. On success return the actual parsed 'Yaml.Value' in
-- 'Right', otherwise return 'SourcePos' of parse error and 'String'
-- describing the error as generated by the @yaml@ package in 'Left'.

pYamlBlock :: BParser (Either (SourcePos, String) Yaml.Value)
pYamlBlock = do
  dpos <- getPosition
  string "---" *> sc' *> eol
  let go = do
        l <- takeWhileP Nothing notNewline
        void (optional eol)
        e <- atEnd
        if e || T.stripEnd l == "---"
          then return []
          else (l :) <$> go
  ls <- go
  case (Yaml.decodeEither . TE.encodeUtf8 . T.intercalate "\n") ls of
    Left err' -> do
      let (apos, err) = splitYamlError (sourceName dpos) err'
      return $ Left (fromMaybe dpos apos, err)
    Right v ->
      return (Right v)

-- | Parse several (possibly zero) blocks in a row.

pBlocks :: BParser [Block Isp]
pBlocks = many pBlock

-- | Parse a single block of markdown document.

pBlock :: BParser (Block Isp)
pBlock = do
  sc
  rlevel <- asks benvRefLevel
  alevel <- L.indentLevel
  done   <- atEnd
  if done || alevel < rlevel then empty else
    case compare alevel (ilevel rlevel) of
      LT -> choice
        [ pThematicBreak
        , pAtxHeading
        , pFencedCodeBlock
        , pUnorderedList
        , pOrderedList
        , pBlockquote
        , pParagraph ]
      _  ->
          pIndentedCodeBlock

-- | Parse a thematic break.

pThematicBreak :: BParser (Block Isp)
pThematicBreak = do
  l'     <- lookAhead nonEmptyLine
  clevel <- ilevel <$> asks benvRefLevel
  let l = T.filter (not . isSpace) l'
  if T.length    l >= 3      &&
     indentLevel l' < clevel &&
     (T.all (== '*') l ||
      T.all (== '-') l ||
      T.all (== '_') l)
    then ThematicBreak <$ nonEmptyLine <* sc
    else empty

-- | Parse an ATX heading.

pAtxHeading :: BParser (Block Isp)
pAtxHeading = do
  (void . lookAhead . try) hashIntro
  withRecovery recover $ do
    hlevel <- length <$> hashIntro
    sc1'
    ispPos <- getPosition
    r <- someTill (satisfy notNewline <?> "heading character") . try $
      optional (sc1' *> some (char '#') *> sc') *> (eof <|> eol)
    let toBlock = case hlevel of
          1 -> Heading1
          2 -> Heading2
          3 -> Heading3
          4 -> Heading4
          5 -> Heading5
          _ -> Heading6
    toBlock (IspSpan ispPos (T.strip (T.pack r))) <$ sc
  where
    hashIntro = count' 1 6 (char '#')
    recover err =
      Heading1 (IspError err) <$ takeWhileP Nothing notNewline <* sc

-- | Parse a fenced code block.

pFencedCodeBlock :: BParser (Block Isp)
pFencedCodeBlock = do
  let p ch = try $ do
        void $ count 3 (char ch)
        n  <- (+ 3) . length <$> many (char ch)
        ml <- optional
          (T.strip <$> someEscapedWith notNewline <?> "info string")
        guard (maybe True (not . T.any (== '`')) ml)
        return
          (ch, n,
             case ml of
               Nothing -> Nothing
               Just l  ->
                 if T.null l
                   then Nothing
                   else Just l)
  alevel <- L.indentLevel
  (ch, n, infoString) <- (p '`' <|> p '~') <* eol
  let content = label "code block content" (option "" nonEmptyLine <* eol)
      closingFence = try . label "closing code fence" $ do
        clevel <- ilevel <$> asks benvRefLevel
        void $ L.indentGuard sc' LT clevel
        void $ count n (char ch)
        (void . many . char) ch
        sc'
        eof <|> eol
  ls <- manyTill content closingFence
  CodeBlock infoString (assembleCodeBlock alevel ls) <$ sc

-- | Parse an indented code block.

pIndentedCodeBlock :: BParser (Block Isp)
pIndentedCodeBlock = do
  alevel <- L.indentLevel
  clevel <- ilevel <$> asks benvRefLevel
  let go ls = do
        immediate <- lookAhead $
          (>= clevel) <$> (sc' *> L.indentLevel)
        eventual  <- lookAhead $
          (>= clevel) <$> (sc *> L.indentLevel)
        if immediate || eventual
          then do
            l        <- option "" nonEmptyLine
            continue <- eol'
            if continue
              then go (l:ls)
              else return (l:ls)
          else return ls
      -- NOTE This is a bit unfortunate, but it's difficult to guarantee
      -- that preceding space is not yet consumed when we get to
      -- interpreting input as an indented code block, so we need to restore
      -- the space this way.
      f x      = T.replicate (unPos alevel - 1) " " <> x
      g []     = []
      g (x:xs) = f x : xs
  ls <- g . reverse . dropWhile isBlank <$> go []
  CodeBlock Nothing (assembleCodeBlock clevel ls) <$ sc

-- | Parse an unorederd list.

pUnorderedList :: BParser (Block Isp)
pUnorderedList = do
  (bullet, bulletPos, minLevel, indLevel) <-
    pListBullet Nothing
  x      <- innerBlocks bulletPos minLevel indLevel
  xs     <- many $ do
    (_, bulletPos', minLevel', indLevel') <-
      pListBullet (Just (bullet, bulletPos))
    innerBlocks bulletPos' minLevel' indLevel'
  return (UnorderedList (normalizeListItems (x:|xs)))
  where
    innerBlocks bulletPos minLevel indLevel = do
      p <- getPosition
      let tooFar = sourceLine p > sourceLine bulletPos <> pos1
          rlevel = slevel minLevel indLevel
      if tooFar || sourceColumn p < minLevel
        then return [if tooFar then emptyParagraph else emptyNaked]
        else subEnv True rlevel pBlocks

-- | Parse a list bullet. Return a tuple with the following components (in
-- order):
--
--     * 'Char' used to represent the bullet
--     * 'SourcePos' at which the bullet was located
--     * the closest column position where content could start
--     * the indentation level after the bullet

pListBullet
  :: Maybe (Char, SourcePos)
     -- ^ Bullet 'Char' and start position of the first bullet in a list
  -> BParser (Char, SourcePos, Pos, Pos)
pListBullet mbullet = try $ do
  pos     <- getPosition
  l       <- (<> mkPos 2) <$> L.indentLevel
  bullet  <-
    case mbullet of
      Nothing -> char '-' <|> char '+' <|> char '*'
      Just (bullet, bulletPos) -> do
        guard (sourceColumn pos >= sourceColumn bulletPos)
        char bullet
  eof <|> sc1
  l'      <- L.indentLevel
  return (bullet, pos, l, l')

-- | Parse an ordered list.

pOrderedList :: BParser (Block Isp)
pOrderedList = do
  (startIx, del, startPos, minLevel, indLevel) <-
    pListIndex Nothing
  x  <- innerBlocks startPos minLevel indLevel
  xs <- manyIndexed (startIx + 1) $ \expectedIx -> do
    (actualIx, _, startPos', minLevel', indLevel') <-
      pListIndex (Just (del, startPos))
    let f blocks =
          if actualIx == expectedIx
            then blocks
            else prependErr
                   startPos'
                   (ListIndexOutOfOrder actualIx expectedIx)
                   blocks
    f <$> innerBlocks startPos' minLevel' indLevel'
  return . OrderedList startIx . normalizeListItems $
    (if startIx <= 999999999
       then x
       else prependErr startPos (ListStartIndexTooBig startIx) x)
    :| xs
  where
    innerBlocks indexPos minLevel indLevel = do
      p <- getPosition
      let tooFar = sourceLine p > sourceLine indexPos <> pos1
          rlevel = slevel minLevel indLevel
      if tooFar || sourceColumn p < minLevel
        then return [if tooFar then emptyParagraph else emptyNaked]
        else subEnv True rlevel pBlocks

-- | Parse a list index. Return a tuple with the following components (in
-- order):
--
--     * 'Word' parsed numeric index
--     * 'Char' used as delimiter after the numeric index
--     * 'SourcePos' at which the index was located
--     * the closest column position where content could start
--     * the indentation level after the index

pListIndex
  :: Maybe (Char, SourcePos)
     -- ^ Delimiter 'Char' and start position of the first index in a list
  -> BParser (Word, Char, SourcePos, Pos, Pos)
pListIndex mstart = try $ do
  pos <- getPosition
  i   <- L.decimal
  del <- case mstart of
    Nothing -> char '.' <|> char ')'
    Just (del, startPos) -> do
      guard (sourceColumn pos >= sourceColumn startPos)
      char del
  l   <- (<> pos1) <$> L.indentLevel
  eof <|> sc1
  l'  <- L.indentLevel
  return (i, del, pos, l, l')

-- | Parse a block quote.

pBlockquote :: BParser (Block Isp)
pBlockquote = do
  minLevel <- try $ do
    minLevel <- (<> pos1) <$> L.indentLevel
    void (char '>')
    eof <|> sc
    l <- L.indentLevel
    return $
      if l > minLevel
        then minLevel <> pos1
        else minLevel
  indLevel <- L.indentLevel
  if indLevel >= minLevel
    then do
      let rlevel = slevel minLevel indLevel
      xs <- subEnv False rlevel pBlocks
      return (Blockquote xs)
    else return (Blockquote [])

-- | Parse a paragraph or naked text (is some cases).

pParagraph :: BParser (Block Isp)
pParagraph = do
  startPos   <- getPosition
  allowNaked <- asks benvAllowNaked
  rlevel     <- asks benvRefLevel
  let go ls = do
        l <- lookAhead (option "" nonEmptyLine)
        broken <- succeeds . lookAhead . try $ do
          sc
          alevel <- L.indentLevel
          guard (alevel < ilevel rlevel)
          unless (alevel < rlevel) . choice $
            [ void (char '>')
            , void pThematicBreak
            , void pAtxHeading
            , void (pListBullet Nothing)
            , void (pListIndex  Nothing) ]
        if isBlank l
          then return (ls, Paragraph)
          else if broken
                 then return (ls, Naked)
                 else do
                   void nonEmptyLine
                   continue <- eol'
                   let ls' = ls . (l:)
                   if continue
                     then go ls'
                     else return (ls', Naked)
  l        <- nonEmptyLine
  continue <- eol'
  (ls, toBlock) <-
    if continue
      then go id
      else return (id, Naked)
  (if allowNaked then toBlock else Paragraph)
    (IspSpan startPos (assembleParagraph (l:ls []))) <$ sc

----------------------------------------------------------------------------
-- Inline parser

-- | Run a given parser on 'Isp'.

runIsp
  :: IParser a         -- ^ The parser to run
  -> Isp               -- ^ Input for the parser
  -> Either (ParseError Char MMarkErr) a -- ^ Result of parsing
runIsp _ (IspError err) = Left err
runIsp p (IspSpan startPos input) =
  snd (runParser' (evalStateT p SpaceChar) pst)
  where
    pst = State
      { stateInput           = input
      , statePos             = nes startPos
      , stateTokensProcessed = 0
      , stateTabWidth        = mkPos 4 }

-- | Parse inlines using settings from given 'InlineConfig'.

pInlines :: InlineConfig -> IParser (NonEmpty Inline)
pInlines InlineConfig {..} =
  if iconfigAllowEmpty
    then nes (Plain "") <$ eof <|> stuff
    else stuff
  where
    stuff = NE.some . label "inline content" . choice $
      [ pCodeSpan                                  ] <>
      [ pInlineLink           | iconfigAllowLinks  ] <>
      [ pImage                | iconfigAllowImages ] <>
      [ try (angel pAutolink) | iconfigAllowLinks  ] <>
      [ pEnclosedInline
      , try pHardLineBreak
      , pPlain ]
    angel = between (char '<') (char '>')

-- | Parse a code span.

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
  put OtherChar
  return r

-- | Parse a link.

pInlineLink :: IParser Inline
pInlineLink = do
  xs     <- between (char '[') (char ']') $
    pInlines def { iconfigAllowLinks = False }
  void (char '(') <* sc
  dest   <- pUri
  mtitle <- optional (sc1 *> pTitle)
  sc <* char ')'
  put OtherChar
  return (Link xs dest mtitle)

-- | Parse an image.

pImage :: IParser Inline
pImage = do
  let nonEmptyDesc = char '!' *> between (char '[') (char ']')
        (pInlines def { iconfigAllowImages = False })
  alt    <- nes (Plain "") <$ string "![]" <|> nonEmptyDesc
  void (char '(') <* sc
  src    <- pUri
  mtitle <- optional (sc1 *> pTitle)
  sc <* char ')'
  put OtherChar
  return (Image alt src mtitle)

-- | Parse a URI.

pUri :: IParser URI
pUri = do
  uri <- between (char '<') (char '>') URI.parser <|> naked
  put OtherChar
  return uri
  where
    naked = do
      startPos <- getPosition
      input    <- takeWhileP Nothing $ \x ->
        not (isSpaceN x || x == ')')
      let pst = State
            { stateInput           = input
            , statePos             = nes startPos
            , stateTokensProcessed = 0
            , stateTabWidth        = mkPos 4 }
      case snd (runParser' (URI.parser <* eof) pst) of
        Left err' ->
          case replaceEof "end of URI literal" err' of
            TrivialError pos us es -> do
              setPosition (NE.head pos)
              failure us es
            FancyError pos xs -> do
              setPosition (NE.head pos)
              fancyFailure xs
        Right x -> return x

-- | Parse a title of a link or an image.

pTitle :: IParser Text
pTitle = choice
  [ p '\"' '\"'
  , p '\'' '\''
  , p '('  ')' ]
  where
    p start end = between (char start) (char end) $
      manyEscapedWith (/= end) "unescaped character"

-- | Parse an autolink.

pAutolink :: IParser Inline
pAutolink = do
  notFollowedBy (char '>') -- empty links don't make sense
  uri <- URI.parser
  put OtherChar
  return $ case isEmailUri uri of
    Nothing ->
      let txt = (nes . Plain . URI.render) uri
      in Link txt uri Nothing
    Just email ->
      let txt  = nes (Plain email)
          uri' = URI.makeAbsolute mailtoScheme uri
      in Link txt uri' Nothing

-- | Parse inline content inside some sort of emphasis, strikeout,
-- superscript, and\/or subscript markup.

pEnclosedInline :: IParser Inline
pEnclosedInline = do
  let noEmpty = def { iconfigAllowEmpty = False }
  st <- choice
    [ pLfdr (DoubleFrame StrongFrame StrongFrame)
    , pLfdr (DoubleFrame StrongFrame EmphasisFrame)
    , pLfdr (SingleFrame StrongFrame)
    , pLfdr (SingleFrame EmphasisFrame)
    , pLfdr (DoubleFrame StrongFrame_ StrongFrame_)
    , pLfdr (DoubleFrame StrongFrame_ EmphasisFrame_)
    , pLfdr (SingleFrame StrongFrame_)
    , pLfdr (SingleFrame EmphasisFrame_)
    , pLfdr (DoubleFrame StrikeoutFrame StrikeoutFrame)
    , pLfdr (DoubleFrame StrikeoutFrame SubscriptFrame)
    , pLfdr (SingleFrame StrikeoutFrame)
    , pLfdr (SingleFrame SubscriptFrame)
    , pLfdr (SingleFrame SuperscriptFrame) ]
  case st of
    SingleFrame x ->
      liftFrame x <$> pInlines noEmpty <* pRfdr x
    DoubleFrame x y -> do
      inlines0  <- pInlines noEmpty
      thisFrame <- pRfdr x <|> pRfdr y
      let thatFrame = if x == thisFrame then y else x
      immediate <- True <$ pRfdr thatFrame <|> pure False
      if immediate
        then (return . liftFrame thatFrame . nes . liftFrame thisFrame) inlines0
        else do
          inlines1 <- pInlines noEmpty
          void (pRfdr thatFrame)
          return . liftFrame thatFrame $
            liftFrame thisFrame inlines0 <| inlines1

-- | Parse an opening markup sequence corresponding to given 'InlineState'

pLfdr :: InlineState -> IParser InlineState
pLfdr st = try $ do
  let dels = inlineStateDel st
  pos <- getPosition
  void (string dels)
  leftChar   <- get
  mrightChar <- lookAhead (optional anyChar)
  let failNow = do
        setPosition pos
        (mmarkErr . NonFlankingDelimiterRun . toNesTokens) dels
  case (leftChar, isTransparent <$> mrightChar) of
    (_, Nothing)          -> failNow
    (_, Just True)        -> failNow
    (RightFlankingDel, _) -> failNow
    (OtherChar, _)        -> failNow
    (SpaceChar, _)        -> return ()
    (LeftFlankingDel, _)  -> return ()
  put LeftFlankingDel
  return st

-- | Parse a closing markdup sequence corresponding to given 'InlineFrame'.

pRfdr :: InlineFrame -> IParser InlineFrame
pRfdr frame = try $ do
  let dels = inlineFrameDel frame
  pos <- getPosition
  void (string dels)
  leftChar   <- get
  mrightChar <- lookAhead (optional anyChar)
  let failNow = do
        setPosition pos
        (mmarkErr . NonFlankingDelimiterRun . toNesTokens) dels
  case (leftChar, mrightChar) of
    (SpaceChar, _) -> failNow
    (LeftFlankingDel, _) -> failNow
    (_, Nothing) -> return ()
    (_, Just rightChar) ->
      if | isTransparent rightChar -> return ()
         | isMarkupChar  rightChar -> return ()
         | otherwise               -> failNow
  put RightFlankingDel
  return frame

-- | Parse a hard line break.

pHardLineBreak :: IParser Inline
pHardLineBreak = do
  void (char '\\')
  eol
  notFollowedBy eof
  sc'
  put SpaceChar
  return LineBreak

-- | Parse plain text.

pPlain :: IParser Inline
pPlain = Plain . T.pack <$> some
  (pEscapedChar <|> pNewline <|> pNonEscapedChar)
  where
    pEscapedChar = escapedChar <* put OtherChar
    pNewline = hidden . try $
      '\n' <$ sc' <* eol <* sc' <* put SpaceChar
    pNonEscapedChar = label "unescaped non-markup character" . choice $
      [ try (char '\\' <* notFollowedBy eol)        <* put OtherChar
      , try (char '!'  <* notFollowedBy (char '[')) <* put SpaceChar
      , try (char '<'  <* notFollowedBy (pAutolink <* char '>')) <* put OtherChar
      , spaceChar                                   <* put SpaceChar
      , satisfy isTrans                             <* put SpaceChar
      , satisfy isOther                             <* put OtherChar ]
    isTrans x = isTransparentPunctuation x && x /= '!'
    isOther x = not (isMarkupChar x) && x /= '\\' && x /= '!' && x /= '<'

----------------------------------------------------------------------------
-- Parsing helpers

nonEmptyLine :: BParser Text
nonEmptyLine = takeWhile1P Nothing notNewline

manyEscapedWith :: MonadParsec e Text m => (Char -> Bool) -> String -> m Text
manyEscapedWith f l = T.pack <$> many (escapedChar <|> (satisfy f <?> l))

someEscapedWith :: MonadParsec e Text m => (Char -> Bool) -> m Text
someEscapedWith f = T.pack <$> some (escapedChar <|> satisfy f)

escapedChar :: MonadParsec e Text m => m Char
escapedChar = try (char '\\' *> satisfy isAsciiPunctuation)
  <?> "escaped character"

sc :: MonadParsec e Text m => m ()
sc = void $ takeWhileP (Just "white space") isSpaceN

sc1 :: MonadParsec e Text m => m ()
sc1 = void $ takeWhile1P (Just "white space") isSpaceN

sc' :: MonadParsec e Text m => m ()
sc' = void $ takeWhileP (Just "white space") isSpace

sc1' :: MonadParsec e Text m => m ()
sc1' = void $ takeWhile1P (Just "white space") isSpace

eol :: MonadParsec e Text m => m ()
eol = void . label "newline" $ choice
  [ string "\n"
  , string "\r\n"
  , string "\r" ]

eol' :: MonadParsec e Text m => m Bool
eol' = option False (True <$ eol)

subEnv :: Bool -> Pos -> BParser a -> BParser a
subEnv benvAllowNaked benvRefLevel = local (const BlockEnv {..})

slevel :: Pos -> Pos -> Pos
slevel a l = if l >= ilevel a then a else l

ilevel :: Pos -> Pos
ilevel = (<> mkPos 4)

----------------------------------------------------------------------------
-- Other helpers

isSpace :: Char -> Bool
isSpace x = x == ' ' || x == '\t'

isSpaceN :: Char -> Bool
isSpaceN x = isSpace x || x == '\n' || x == '\r'

notNewline :: Char -> Bool
notNewline x = x /= '\n' && x /= '\r'

isBlank :: Text -> Bool
isBlank = T.all isSpace

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

isAsciiPunctuation :: Char -> Bool
isAsciiPunctuation x =
  (x >= '!' && x <= '/') ||
  (x >= ':' && x <= '@') ||
  (x >= '[' && x <= '`') ||
  (x >= '{' && x <= '~')

isTransparentPunctuation :: Char -> Bool
isTransparentPunctuation = \case
  '!' -> True
  '"' -> True
  '(' -> True
  ')' -> True
  ',' -> True
  '-' -> True
  '.' -> True
  ':' -> True
  ';' -> True
  '?' -> True
  '{' -> True
  '}' -> True
  '–' -> True
  '—' -> True
  _   -> False

isTransparent :: Char -> Bool
isTransparent x = Char.isSpace x || isTransparentPunctuation x

nes :: a -> NonEmpty a
nes a = a :| []

assembleCodeBlock :: Pos -> [Text] -> Text
assembleCodeBlock indent ls = T.unlines (stripIndent indent <$> ls)

assembleParagraph :: [Text] -> Text
assembleParagraph = go
  where
    go []     = ""
    go [x]    = T.dropWhileEnd isSpace x
    go (x:xs) = x <> "\n" <> go xs

indentLevel :: Text -> Pos
indentLevel = T.foldl' f pos1 . T.takeWhile isSpace
  where
    f n ch
      | ch == ' '  = n <> pos1
      | ch == '\t' = n <> mkPos 4
      | otherwise  = n

stripIndent :: Pos -> Text -> Text
stripIndent indent txt = T.drop m txt
  where
    m = snd $ T.foldl' f (0, 0) (T.takeWhile p txt)
    p x = isSpace x || x == '>'
    f (!j, !n) ch
      | j  >= i    = (j, n)
      | ch == ' '  = (j + 1, n + 1)
      | ch == '\t' = (j + 4, n + 1)
      | otherwise  = (j, n)
    i = unPos indent - 1

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

inlineStateDel :: InlineState -> Text
inlineStateDel = \case
  SingleFrame x   -> inlineFrameDel x
  DoubleFrame x y -> inlineFrameDel x <> inlineFrameDel y

liftFrame :: InlineFrame -> NonEmpty Inline -> Inline
liftFrame = \case
  StrongFrame      -> Strong
  EmphasisFrame    -> Emphasis
  StrongFrame_     -> Strong
  EmphasisFrame_   -> Emphasis
  StrikeoutFrame   -> Strikeout
  SubscriptFrame   -> Subscript
  SuperscriptFrame -> Superscript

replaceEof :: String -> ParseError Char e -> ParseError Char e
replaceEof altLabel = \case
  TrivialError pos us es -> TrivialError pos (f <$> us) (E.map f es)
  FancyError   pos xs    -> FancyError pos xs
  where
    f EndOfInput = Label (NE.fromList altLabel)
    f x          = x

mmarkErr :: MonadParsec MMarkErr s m => MMarkErr -> m a
mmarkErr = fancyFailure . E.singleton . ErrorCustom

toNesTokens :: Text -> NonEmpty Char
toNesTokens = NE.fromList . T.unpack

isEmailUri :: URI -> Maybe Text
isEmailUri uri =
  case URI.unRText <$> URI.uriPath uri of
    [x] ->
      if Email.isValid (TE.encodeUtf8 x) &&
          (isNothing (URI.uriScheme uri) ||
           URI.uriScheme uri == Just mailtoScheme)
        then Just x
        else Nothing
    _ -> Nothing

mailtoScheme :: URI.RText 'URI.Scheme
mailtoScheme = fromJust (URI.mkScheme "mailto")

splitYamlError :: FilePath -> String -> (Maybe SourcePos, String)
splitYamlError file str = maybe (Nothing, str) (first pure) (parseMaybe p str)
  where
    p :: Parsec Void String (SourcePos, String)
    p = do
      void (string "YAML parse exception at line ")
      l <- mkPos . (+ 2) <$> L.decimal
      void (string ", column ")
      c <- mkPos . (+ 1) <$> L.decimal
      void (string ":\n")
      r <- takeRest
      return (SourcePos file l c, r)

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         =
  error "Text.MMark.Parser.fromRight: the impossible happened"

manyIndexed :: (Alternative m, Num n) => n -> (n -> m a) -> m [a]
manyIndexed n' m = go n'
  where
    go !n = liftA2 (:) (m n) (go (n + 1)) <|> pure []

emptyParagraph :: Block Isp
emptyParagraph = Paragraph (IspSpan (initialPos "") "")

emptyNaked :: Block Isp
emptyNaked = Naked (IspSpan (initialPos "") "")

normalizeListItems :: NonEmpty [Block Isp] -> NonEmpty [Block Isp]
normalizeListItems xs' =
  if getAny $ foldMap (foldMap (Any . isParagraph)) (drop 1 x :| xs)
    then fmap toParagraph <$> xs'
    else case x of
           [] -> xs'
           (y:ys) -> r $ (toNaked y : ys) :| xs
  where
    (x:|xs) = r xs'
    r = NE.reverse . fmap reverse
    isParagraph = \case
      OrderedList _ _ -> False
      UnorderedList _ -> False
      Naked         _ -> False
      _               -> True
    toParagraph (Naked inner) = Paragraph inner
    toParagraph other         = other
    toNaked (Paragraph inner) = Naked inner
    toNaked other             = other

-- | Convert @'Either' a b@ to @'Pair' a b@.

e2p :: Either a b -> Pair a b
e2p = \case
  Left  a -> PairL a
  Right b -> PairR (b:)

succeeds :: Alternative m => m () -> m Bool
succeeds m = True <$ m <|> pure False

prependErr :: SourcePos -> MMarkErr -> [Block Isp] -> [Block Isp]
prependErr pos custom blocks = Naked (IspError err) : blocks
  where
    err = FancyError (nes pos) (E.singleton $ ErrorCustom custom)
