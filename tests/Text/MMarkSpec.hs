{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMarkSpec (spec) where

import Data.Char
import Data.Monoid
import Data.Text (Text)
import Test.Hspec
import Text.MMark ((.&+))
import Text.MMark.Extension (Inline (..))
import Text.MMark.TestUtils
import qualified Data.Text            as T
import qualified Text.MMark           as MMark
import qualified Text.MMark.Extension as Ext

spec :: Spec
spec = parallel $ do
  describe "parse and render" $
    context "2.2 Tabs" $ do
      it "CM1" $
        "\tfoo\tbaz\t\tbim" ==->
          "<pre><code>foo\tbaz\t\tbim\n</code></pre>\n"
      it "CM2" $
        "  \tfoo\tbaz\t\tbim" ==->
          "<pre><code>foo\tbaz\t\tbim\n</code></pre>\n"
      it "CM3" $
        "    a\ta\n    ὐ\ta" ==->
          "<pre><code>a\ta\nὐ\ta\n</code></pre>\n"
      xit "CM4" $ -- FIXME pending lists
        "  - foo\n\n\tbar" ==->
          "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      xit "CM5" $ -- FIXME pending lists
        "- foo\n\n\t\tbar" ==->
          "<ul>\n<li>\n<p>foo</p>\n<pre><code>  bar\n</code></pre>\n</li>\n</ul>\n"
      xit "CM6" $ -- FIXME pending blockquotes
        ">\t\tfoo" ==->
          "<blockquote>\n<pre><code>  foo\n</code></pre>\n</blockquote>\n"
      xit "CM7" $ -- FIXME pending lists
        "-\t\tfoo" ==->
          "<ul>\n<li>\n<pre><code>  foo\n</code></pre>\n</li>\n</ul>\n"
      it "CM8" $
        "    foo\n\tbar" ==->
          "<pre><code>foo\nbar\n</code></pre>\n"
      xit "CM9" $ -- FIXME pending lists
        " - foo\n   - bar\n\t - baz" ==->
          "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>baz</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM10" $
        "#\tFoo" ==-> "<h1>Foo</h1>\n"
      it "CM11" $
        "*\t*\t*\t" ==-> "<hr>\n"
  describe "useExtension" $
    it "applies given extension" $ do
      doc <- mkDoc "Here we go."
      toText (MMark.useExtension (append_ext "..") doc) `shouldBe`
        "<p>Here we go...</p>\n"
  describe "useExtensions" $
    it "applies extensions in the right order" $ do
      doc <- mkDoc "Here we go."
      let exts =
            [ append_ext "3"
            , append_ext "2"
            , append_ext "1" ]
      toText (MMark.useExtensions exts doc) `shouldBe`
        "<p>Here we go.123</p>\n"
  describe "runScanner and scanner" $
    it "extracts information from markdown document" $ do
      doc <- mkDoc "Here we go, pals."
      let n = MMark.runScanner doc (length_scan (const True)) 0
      n `shouldBe` 17
  describe "(.&+)" $
    it "combines scanners" $ do
      doc <- mkDoc "Here we go, pals."
      let scan = length_scan (const True)
            .&+ length_scan isSpace
            .&+ length_scan isPunctuation
          r = MMark.runScanner doc scan ((0, 0), 0)
      r `shouldBe` ((17, 3), 2)
  describe "projectYaml" $ do
    context "when document does not contain a YAML section" $
      it "returns Nothing" $ do
        doc <- mkDoc "Here we go."
        MMark.projectYaml doc `shouldBe` Nothing
    context "when document contains a YAML section" $
      it "return the YAML section" $ do
        doc <- mkDoc "---\nx: 100\ny: 200\n---Here we go."
        MMark.projectYaml doc `shouldBe` Nothing -- FIXME when we support YAML blocks

----------------------------------------------------------------------------
-- Testing extensions

-- | Append given text to all 'Plain' blocks.

append_ext :: Text -> MMark.Extension
append_ext y = Ext.inlineTrans $ \case
  Plain x -> Plain (x <> y)
  other   -> other

----------------------------------------------------------------------------
-- Testing scanners

-- | Scan total number of characters satisfying a predicate in all 'Plain'
-- inlines.

length_scan :: (Char -> Bool) -> MMark.Scanner Int
length_scan p = Ext.scanner $ \n block ->
  getSum $ Sum n <> foldMap (foldMap f) block
  where
    f (Plain txt) = (Sum . T.length) (T.filter p txt)
    f _           = mempty
