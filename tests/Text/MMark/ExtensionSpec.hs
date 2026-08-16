{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.MMark.ExtensionSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Lucid qualified as L
import Test.Hspec
import Test.QuickCheck hiding (collect)
import Text.MMark (MMark)
import Text.MMark qualified as MMark
import Text.MMark.Render qualified as Render
import Text.MMark.TestUtils
import Text.MMark.Trans (Block (..), Bni, Inline (..), Span (..), Trans, TransT)
import Text.MMark.Trans qualified as Trans
import Text.Megaparsec (errorBundlePretty)
import Text.URI qualified as URI

spec :: Spec
spec = parallel $ do
  describe "bottomUpBlocks" $ do
    it "works" $ do
      doc <- mkDoc "# My heading"
      trans h1_to_h2 doc
        `shouldReturn` "<h2 id=\"my-heading\">My heading</h2>\n"
    it "reaches nested block structures" $ do
      doc <- mkDoc "* # My heading"
      trans h1_to_h2 doc
        `shouldReturn` "<ul>\n<li>\n<h2 id=\"my-heading\">My heading</h2>\n</li>\n</ul>\n"
    it "visits the innermost blocks first" $ do
      doc <- mkDoc "> * a"
      order (Trans.bottomUpBlocks . note) doc
        `shouldReturn` ["Naked", "UnorderedList", "Blockquote"]
  describe "topDownBlocks" $
    it "visits the outermost blocks first" $ do
      doc <- mkDoc "> * a"
      order (Trans.topDownBlocks . note) doc
        `shouldReturn` ["Blockquote", "UnorderedList", "Naked"]
  describe "blockRender" $ do
    it "works" $ do
      doc <- mkDoc "# My heading"
      toTextWith add_h1_content doc
        `shouldBe` "<h1 data-content=\"My heading\" id=\"my-heading\">My heading</h1>\n"
    it "extensions can affect nested block structures" $ do
      doc <- mkDoc "* # Something"
      toTextWith add_h1_content doc
        `shouldBe` "<ul>\n<li>\n<h1 data-content=\"Something\" id=\"something\">Something</h1>\n</li>\n</ul>\n"
  describe "bottomUpInlines" $ do
    it "works" $ do
      doc <- mkDoc "# My *heading*"
      trans (Trans.bottomUpInlines em_to_strong) doc
        `shouldReturn` "<h1 id=\"my-heading\">My <strong>heading</strong></h1>\n"
    it "reaches nested inline structures" $ do
      doc <- mkDoc "# My ~*heading*~"
      trans (Trans.bottomUpInlines em_to_strong) doc
        `shouldReturn` "<h1 id=\"my-heading\">My <sub><strong>heading</strong></sub></h1>\n"
  describe "inlineRender" $ do
    it "works" $ do
      doc <- mkDoc "# My *heading*"
      toTextWith (add_em_class "foo") doc
        `shouldBe` "<h1 id=\"my-heading\">My <em class=\"foo\">heading</em></h1>\n"
    it "extensions can affect nested inline structures" $ do
      doc <- mkDoc "[*heading*](/url)"
      toTextWith (add_em_class "foo") doc
        `shouldBe` "<p><a href=\"/url\"><em class=\"foo\">heading</em></a></p>\n"
  describe "spans" $ do
    it "cover a block and the white space that follows it" $ do
      doc <- mkDoc "# One\n\nTwo three."
      spansOf doc `shouldBe` [Span 0 7, Span 7 17]
    it "point at the source an inline was parsed from" $ do
      doc <- mkDoc "a *b* c"
      inlineSpansOf doc `shouldBe` [Span 0 2, Span 2 5, Span 5 7]
  describe "report" $ do
    it "reports every offending node, not only the first" $ do
      doc <- mkDoc "*a* and *b*"
      errs <- transErrors (Trans.bottomUpInlines noEmphasis) doc
      errs `shouldBe` ["1:1:", "1:9:"]
    it "renders errors against the source of the document" $ do
      doc <- mkDoc "*a*"
      errs <- transErrorText (Trans.bottomUpInlines noEmphasis) doc
      errs `shouldSatisfy` T.isInfixOf "no emphasis allowed"
    it "orders the errors by position, not by when they were reported" $ do
      doc <- mkDoc "one *a* two *b* three"
      errs <- transErrors reportBackwards doc
      errs `shouldBe` ["1:5:", "1:13:"]
    it "renders an error against the right source line whatever the order" $ do
      doc <- mkDoc "one *a* two *b* three"
      txt <- transErrorText reportBackwards doc
      txt `shouldSatisfy` T.isInfixOf "1:5:"
      txt `shouldSatisfy` T.isInfixOf "1:13:"
  describe "abort" $
    it "gives up but keeps the errors reported before it" $ do
      doc <- mkDoc "*a* and *b*"
      errs <- transErrors (Trans.bottomUpInlines noEmphasisAbort) doc
      errs `shouldBe` ["1:1:"]
  describe "runCheck" $ do
    it "runs the check once, whatever the document contains" $ do
      doc <- mkDoc "one\n\ntwo\n\nthree\n\nfour"
      let check = Trans.report (Span 0 3) "just once"
      case MMark.runCheck check doc of
        Right () -> expectationFailure "the check was expected to report"
        Left errs -> length (T.lines (T.pack (errorBundlePretty errs))) `shouldBe` 5
    it "gives back what the check returns when it reports nothing" $ do
      doc <- mkDoc "one"
      MMark.runCheck (return (42 :: Int)) doc `shouldBe` Right 42
    it "resolves positions against the document" $ do
      doc <- mkDoc "one\ntwo\nthree"
      let check = Trans.report (Span 8 13) "here"
      case MMark.runCheck check doc of
        Right () -> expectationFailure "the check was expected to report"
        Left errs -> T.pack (errorBundlePretty errs) `shouldSatisfy` T.isInfixOf "3:1:"
  describe "runCheckM" $
    it "can perform effects" $ do
      doc <- mkDoc "one"
      ref <- newIORef (0 :: Int)
      _ <- MMark.runCheckM (liftIO (modifyIORef ref (+ 1))) doc
      readIORef ref `shouldReturn` 1
  describe "runTransM" $
    it "can perform effects" $ do
      doc <- mkDoc "# a\n\n# b"
      ref <- newIORef []
      _ <- MMark.runTransM (collect ref) doc
      reverse <$> readIORef ref `shouldReturn` ["a", "b"]
  describe "asPlainText" $ do
    let f x = Trans.asPlainText (x :| [])
        sp = Span 0 0
    context "with Plain" $
      it "works" $
        property $ \txt ->
          f (Plain sp txt) `shouldBe` txt
    context "with LineBreak"
      $ it "works"
      $ f (LineBreak sp) `shouldBe` "\n"
    context "with Emphasis" $
      it "works" $
        property $ \txt ->
          f (Emphasis sp $ Plain sp txt :| []) `shouldBe` txt
    context "with Strong" $
      it "works" $
        property $ \txt ->
          f (Strong sp $ Plain sp txt :| []) `shouldBe` txt
    context "with Strikeout" $
      it "works" $
        property $ \txt ->
          f (Strikeout sp $ Plain sp txt :| []) `shouldBe` txt
    context "with Subscript" $
      it "works" $
        property $ \txt ->
          f (Subscript sp $ Plain sp txt :| []) `shouldBe` txt
    context "with Superscript" $
      it "works" $
        property $ \txt ->
          f (Superscript sp $ Plain sp txt :| []) `shouldBe` txt
    context "with CodeSpan" $
      it "works" $
        property $ \txt ->
          f (CodeSpan sp txt) `shouldBe` txt
    context "with Link" $
      it "works" $
        property $ \txt uri ->
          f (Link sp (Plain sp txt :| []) uri Nothing) `shouldBe` txt
    context "with Image" $
      it "works" $
        property $ \txt uri ->
          f (Image sp (Plain sp txt :| []) uri Nothing) `shouldBe` txt
  describe "headerId"
    $ it "works"
    $ Trans.headerId (Plain (Span 0 0) "Something like that" :| [])
      `shouldBe` "something-like-that"
  describe "headerFragment" $
    it "generates URIs with just that fragment" $
      property $ \fragment -> do
        let uri = Trans.headerFragment fragment
        frag <- URI.mkFragment fragment
        URI.uriScheme uri `shouldBe` Nothing
        URI.uriAuthority uri `shouldBe` Left False
        URI.uriPath uri `shouldBe` Nothing
        URI.uriQuery uri `shouldBe` []
        URI.uriFragment uri `shouldBe` Just frag

----------------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

----------------------------------------------------------------------------
-- Testing extensions

-- | Convert H1 headings into H2 headings.
h1_to_h2 :: Bni -> Trans Bni
h1_to_h2 = Trans.bottomUpBlocks $ \case
  Heading1 ann inner -> return (Heading2 ann inner)
  other -> return other

-- | Add a data attribute calculated based on plain text contents of the
-- level 1 heading to test the 'Render.getOis' thing and 'Render.blockRender' in
-- general.
add_h1_content :: MMark.RenderExtension
add_h1_content = Render.blockRender $ \old block ->
  case block of
    Heading1 ann inner ->
      L.with
        (old (Heading1 ann inner))
        [L.data_ "content" (Trans.asPlainText . Render.getOis . fst $ inner)]
    other -> old other

-- | Convert all 'Emphasis' to 'Strong'.
em_to_strong :: Inline -> Trans Inline
em_to_strong = \case
  Emphasis ann inner -> return (Strong ann inner)
  other -> return other

-- | Report every 'Emphasis' of a block, last one first, so that the errors
-- are reported in the opposite of document order.
reportBackwards :: Bni -> Trans Bni
reportBackwards b = b <$ mapM_ report (reverse (spansOfEmphases b))
  where
    report spn = Trans.report spn "emphasis"
    spansOfEmphases = foldMap (foldMap go)
    go = \case
      Emphasis spn xs -> spn : foldMap go xs
      Strong _ xs -> foldMap go xs
      other -> const [] other

-- | Report every 'Emphasis' and carry on.
noEmphasis :: Inline -> Trans Inline
noEmphasis i = case i of
  Emphasis ann _ -> i <$ Trans.report ann "no emphasis allowed"
  other -> return other

-- | Report the first 'Emphasis' and give up.
noEmphasisAbort :: Inline -> Trans Inline
noEmphasisAbort i = case i of
  Emphasis ann _ -> Trans.abort ann "no emphasis allowed"
  other -> return other

-- | Record the plain text of every heading, in an effect.
collect :: IORef [Text] -> Bni -> TransT IO Bni
collect ref = Trans.bottomUpBlocks $ \b -> case b of
  Heading1 _ inner -> do
    liftIO (modifyIORef ref (Trans.asPlainText inner :))
    return b
  other -> return other

-- | Record the name of the constructor of every block visited.
note :: IORef [String] -> Bni -> TransT IO Bni
note ref b = b <$ liftIO (modifyIORef ref (con b :))
  where
    con = \case
      Blockquote {} -> "Blockquote"
      UnorderedList {} -> "UnorderedList"
      OrderedList {} -> "OrderedList"
      Paragraph {} -> "Paragraph"
      Naked {} -> "Naked"
      Heading1 {} -> "Heading1"
      _ -> "other"

-- | Add given class to all 'Emphasis' things.
add_em_class :: Text -> MMark.RenderExtension
add_em_class given = Render.inlineRender $ \old inline ->
  case inline of
    Emphasis ann inner -> L.with (old (Emphasis ann inner)) [L.class_ given]
    other -> old other

----------------------------------------------------------------------------
-- Helpers

-- | Apply a pure transformation and render the result.
trans :: (Bni -> Trans Bni) -> MMark -> IO Text
trans f doc = case MMark.runTrans f doc of
  Left errs -> fail (errorBundlePretty errs)
  Right doc' -> return (toText doc')

-- | Apply a transformation that is expected to fail and return the
-- position of every error it reported.
transErrors :: (Bni -> Trans Bni) -> MMark -> IO [String]
transErrors f doc = case MMark.runTrans f doc of
  Left errs -> return (filter (isSuffixOf ":") (words (errorBundlePretty errs)))
  Right _ -> fail "the transformation was expected to fail"

-- | Like 'transErrors', but return the rendered errors themselves.
transErrorText :: (Bni -> Trans Bni) -> MMark -> IO Text
transErrorText f doc = case MMark.runTrans f doc of
  Left errs -> return (T.pack (errorBundlePretty errs))
  Right _ -> fail "the transformation was expected to fail"

-- | Run a transformation that records the order in which nodes are visited.
order :: (IORef [String] -> Bni -> TransT IO Bni) -> MMark -> IO [String]
order f doc = do
  ref <- newIORef []
  _ <- MMark.runTransM (f ref) doc
  reverse <$> readIORef ref

-- | The spans of the top-level blocks of a document.
spansOf :: MMark -> [Span]
spansOf doc = MMark.runScanner (MMark.scanner [] (\acc b -> acc ++ [Trans.blockSpan b])) doc

-- | The spans of the inlines of the last block of a document.
inlineSpansOf :: MMark -> [Span]
inlineSpansOf doc =
  MMark.runScanner (MMark.scanner [] (\acc b -> acc ++ foldMap (fmap Trans.inlineSpan . NE.toList) b)) doc
