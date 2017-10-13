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
  describe "parse and render" $ do
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
    context "3.1 Precedence" $
      xit "CM12" $ -- FIXME pending lists
        "- `one\n- two`" ==->
          "<ul>\n<li>`one</li>\n<li>two`</li>\n</ul>\n"
    context "4.1 Thematic breaks" $ do
      it "CM13" $
        "***\n---\n___" ==-> "<hr>\n<hr>\n<hr>\n"
      it "CM14" $
        "+++" ==-> "<p>+++</p>\n"
      it "CM15" $
        "===" ==-> "<p>===</p>\n"
      it "CM16" $
        "--\n**\n__" ==-> "<p>--\n**\n__</p>\n"
      it "CM17" $
        " ***\n  ***\n   ***" ==-> "<hr>\n<hr>\n<hr>\n"
      it "CM18" $
        "    ***" ==-> "<pre><code>***\n</code></pre>\n"
      it "CM19" $
        "Foo\n    ***" ==-> "<p>Foo\n***</p>\n"
      it "CM20" $
        "_____________________________________" ==->
          "<hr>\n"
      it "CM21" $
        " - - -" ==-> "<hr>\n"
      it "CM22" $
        " **  * ** * ** * **" ==-> "<hr>\n"
      it "CM23" $
        "-     -      -      -" ==-> "<hr>\n"
      it "CM24" $
        "- - - -    " ==-> "<hr>\n"
      it "CM25" $
        "_ _ _ _ a\n\na------\n\n---a---" ==->
          "<p>_ _ _ _ a</p>\n<p>a------</p>\n<p>---a---</p>\n"
      it "CM26" $
        " *-*" ==-> "<p><em>-</em></p>\n"
      xit "CM27" $ -- FIXME pending lists
        "- foo\n***\n- bar" ==->
         "<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>\n"
      it "CM28" $
        "Foo\n***\nbar" ==->
          "<p>Foo</p>\n<hr>\n<p>bar</p>\n"
      xit "CM29" $ -- FIXME pending setext headings
        "Foo\n---\nbar" ==->
          "<h2>Foo</h2>\n<p>bar</p>\n"
      xit "CM30" $ -- FIXME pending lists
        "* Foo\n* * *\n* Bar" ==->
          "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n<ul>\n<li>Bar</li>\n</ul>\n"
      xit "CM31" $ -- FIXME pending lists
        "- Foo\n- * * *" ==->
          "<ul>\n<li>Foo</li>\n<li>\n<hr />\n</li>\n</ul>\n"
    context "4.2 ATX headings" $ do
      it "CM32" $
        "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo" ==->
          "<h1>foo</h1>\n<h2>foo</h2>\n<h3>foo</h3>\n<h4>foo</h4>\n<h5>foo</h5>\n<h6>foo</h6>\n"
      it "CM33" $
        "####### foo" ==-> "<p>####### foo</p>\n"
      it "CM34" $
        "#5 bolt\n\n#hashtag" ==-> "<p>#5 bolt</p>\n<p>#hashtag</p>\n"
      it "CM35" $
        "\\## foo" ==-> "<p>## foo</p>\n"
      it "CM36" $
        "# foo *bar* \\*baz\\*" ==-> "<h1>foo <em>bar</em> *baz*</h1>\n"
      it "CM37" $
        "#                  foo                     " ==->
          "<h1>foo</h1>\n"
      it "CM38" $
        " ### foo\n  ## foo\n   # foo" ==->
          "<h3>foo</h3>\n<h2>foo</h2>\n<h1>foo</h1>\n"
      it "CM39" $
        "    # foo" ==-> "<pre><code># foo\n</code></pre>\n"
      it "CM40" $
        "foo\n    # bar" ==-> "<p>foo\n# bar</p>\n"
      it "CM41" $
        "## foo ##\n  ###   bar    ###" ==->
          "<h2>foo</h2>\n<h3>bar</h3>\n"
      it "CM42" $
        "# foo ##################################\n##### foo ##" ==->
          "<h1>foo</h1>\n<h5>foo</h5>\n"
      it "CM43" $
        "### foo ###     " ==-> "<h3>foo</h3>\n"
      it "CM44" $
        "### foo ### b" ==-> "<h3>foo ### b</h3>\n"
      it "CM45" $
        "# foo#" ==-> "<h1>foo#</h1>\n"
      it "CM46" $
        "### foo \\###\n## foo #\\##\n# foo \\#" ==->
          "<h3>foo ###</h3>\n<h2>foo ###</h2>\n<h1>foo #</h1>\n"
      it "CM47" $
        "****\n## foo\n****" ==->
          "<hr>\n<h2>foo</h2>\n<hr>\n"
      it "CM48" $
        "Foo bar\n# baz\nBar foo" ==->
          "<p>Foo bar</p>\n<h1>baz</h1>\n<p>Bar foo</p>\n"
      it "CM49" $
        "## \n#\n### ###" ==->
          "<h2></h2>\n<h1></h1>\n<h3></h3>\n"
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
