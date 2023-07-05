{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.MMark.ExtensionSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Lucid qualified as L
import Test.Hspec
import Test.QuickCheck
import Text.MMark qualified as MMark
import Text.MMark.Extension (Block (..), Inline (..))
import Text.MMark.Extension qualified as Ext
import Text.MMark.TestUtils
import Text.URI qualified as URI

spec :: Spec
spec = parallel $ do
  describe "blockTrans" $ do
    it "works" $ do
      doc <- mkDoc "# My heading"
      toText (MMark.useExtension h1_to_h2 doc)
        `shouldBe` "<h2 id=\"my-heading\">My heading</h2>\n"
    it "extensions can affect nested block structures" $ do
      doc <- mkDoc "* # My heading"
      toText (MMark.useExtension h1_to_h2 doc)
        `shouldBe` "<ul>\n<li>\n<h2 id=\"my-heading\">My heading</h2>\n</li>\n</ul>\n"
  describe "blockRender" $ do
    it "works" $ do
      doc <- mkDoc "# My heading"
      toText (MMark.useExtension add_h1_content doc)
        `shouldBe` "<h1 data-content=\"My heading\" id=\"my-heading\">My heading</h1>\n"
    it "extensions can affect nested block structures" $ do
      doc <- mkDoc "* # Something"
      toText (MMark.useExtension add_h1_content doc)
        `shouldBe` "<ul>\n<li>\n<h1 data-content=\"Something\" id=\"something\">Something</h1>\n</li>\n</ul>\n"
  describe "inlineTrans" $ do
    it "works" $ do
      doc <- mkDoc "# My *heading*"
      toText (MMark.useExtension em_to_strong doc)
        `shouldBe` "<h1 id=\"my-heading\">My <strong>heading</strong></h1>\n"
    it "extensions can affect nested inline structures" $ do
      doc <- mkDoc "# My ~*heading*~"
      toText (MMark.useExtension em_to_strong doc)
        `shouldBe` "<h1 id=\"my-heading\">My <sub><strong>heading</strong></sub></h1>\n"
  describe "inlineRender" $ do
    it "works" $ do
      doc <- mkDoc "# My *heading*"
      toText (MMark.useExtension (add_em_class "foo") doc)
        `shouldBe` "<h1 id=\"my-heading\">My <em class=\"foo\">heading</em></h1>\n"
    it "extensions can affect nested inline structures" $ do
      doc <- mkDoc "[*heading*](/url)"
      toText (MMark.useExtension (add_em_class "foo") doc)
        `shouldBe` "<p><a href=\"/url\"><em class=\"foo\">heading</em></a></p>\n"
  describe "asPlainText" $ do
    let f x = Ext.asPlainText (x :| [])
    context "with Plain" $
      it "works" $
        property $ \txt ->
          f (Plain txt) `shouldBe` txt
    context "with LineBreak" $
      it "works" $
        f LineBreak `shouldBe` "\n"
    context "with Emphasis" $
      it "works" $
        property $ \txt ->
          f (Emphasis $ Plain txt :| []) `shouldBe` txt
    context "with Strong" $
      it "works" $
        property $ \txt ->
          f (Strong $ Plain txt :| []) `shouldBe` txt
    context "with Strikeout" $
      it "works" $
        property $ \txt ->
          f (Strikeout $ Plain txt :| []) `shouldBe` txt
    context "with Subscript" $
      it "works" $
        property $ \txt ->
          f (Subscript $ Plain txt :| []) `shouldBe` txt
    context "with Superscript" $
      it "works" $
        property $ \txt ->
          f (Superscript $ Plain txt :| []) `shouldBe` txt
    context "with CodeSpan" $
      it "works" $
        property $ \txt ->
          f (CodeSpan txt) `shouldBe` txt
    context "with Link" $
      it "works" $
        property $ \txt uri ->
          f (Link (Plain txt :| []) uri Nothing) `shouldBe` txt
    context "with Image" $
      it "works" $
        property $ \txt uri ->
          f (Image (Plain txt :| []) uri Nothing) `shouldBe` txt
  describe "headerId" $
    it "works" $
      Ext.headerId (Plain "Something like that" :| [])
        `shouldBe` "something-like-that"
  describe "headerFragment" $
    it "generates URIs with just that fragment" $
      property $ \fragment -> do
        let uri = Ext.headerFragment fragment
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
h1_to_h2 :: MMark.Extension
h1_to_h2 = Ext.blockTrans $ \case
  Heading1 inner -> Heading2 inner
  other -> other

-- | Add a data attribute calculated based on plain text contents of the
-- level 1 heading to test the 'Ext.getOis' thing and 'Ext.blockRender' in
-- general.
add_h1_content :: MMark.Extension
add_h1_content = Ext.blockRender $ \old block ->
  case block of
    Heading1 inner ->
      L.with
        (old (Heading1 inner))
        [L.data_ "content" (Ext.asPlainText . Ext.getOis . fst $ inner)]
    other -> old other

-- | Convert all 'Emphasis' to 'Strong'.
em_to_strong :: MMark.Extension
em_to_strong = Ext.inlineTrans $ \case
  Emphasis inner -> Strong inner
  other -> other

-- | Add given class to all 'Emphasis' things.
add_em_class :: Text -> MMark.Extension
add_em_class given = Ext.inlineRender $ \old inline ->
  case inline of
    Emphasis inner -> L.with (old (Emphasis inner)) [L.class_ given]
    other -> old other
