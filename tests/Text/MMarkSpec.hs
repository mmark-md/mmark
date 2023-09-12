{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMarkSpec (spec) where

import Control.Foldl qualified as L
import Data.Aeson
import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Monoid
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Lucid
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.MMark (MMarkErr (..))
import Text.MMark qualified as MMark
import Text.MMark.Extension (Inline (..))
import Text.MMark.Extension qualified as Ext
import Text.MMark.TestUtils
import Text.Megaparsec (ErrorFancy (..))

-- NOTE This test suite is mostly based on (sometimes altered) examples from
-- the Common Mark specification. We use the version 0.28 (2017-08-01),
-- which can be found online here:
--
-- <http://spec.commonmark.org/0.28/>

spec :: Spec
spec = parallel $ do
  describe "parse and render" $ do
    context "2.2 Tabs" $ do
      it "CM1" $
        "\tfoo\tbaz\t\tbim"
          ==-> "<pre><code>foo\tbaz\t\tbim\n</code></pre>\n"
      it "CM2" $
        "  \tfoo\tbaz\t\tbim"
          ==-> "<pre><code>foo\tbaz\t\tbim\n</code></pre>\n"
      it "CM3" $
        "    a\ta\n    ὐ\ta"
          ==-> "<pre><code>a\ta\nὐ\ta\n</code></pre>\n"
      it "CM4" $
        "  - foo\n\n\tbar"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      it "CM5" $
        "- foo\n\n\t\tbar"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<pre><code>  bar\n</code></pre>\n</li>\n</ul>\n"
      it "CM6" $
        ">\t\tfoo"
          ==-> "<blockquote>\n<pre><code>  foo\n</code></pre>\n</blockquote>\n"
      it "CM7" $
        "-\t\tfoo"
          ==-> "<ul>\n<li>\n<pre><code>  foo\n</code></pre>\n</li>\n</ul>\n"
      it "CM8" $
        "    foo\n\tbar"
          ==-> "<pre><code>foo\nbar\n</code></pre>\n"
      it "CM9" $
        " - foo\n   - bar\n\t - baz"
          ==-> "<ul>\n<li>\nfoo\n<ul>\n<li>\nbar\n<ul>\n<li>\nbaz\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM10" $
        "#\tFoo" ==-> "<h1 id=\"foo\">Foo</h1>\n"
      it "CM11" $
        "*\t*\t*\t" ==-> "<hr>\n"
    context "3.1 Precedence" $
      it "CM12" $
        let s = "- `one\n- two`"
         in s
              ~~-> [ err 6 (ueib <> etok '`' <> ecsc),
                     err 13 (ueib <> etok '`' <> ecsc)
                   ]
    context "4.1 Thematic breaks" $ do
      it "CM13" $
        "***\n---\n___" ==-> "<hr>\n<hr>\n<hr>\n"
      it "CM14" $
        "+++" ==-> "<p>+++</p>\n"
      it "CM15" $
        "===" ==-> "<p>===</p>\n"
      it "CM16" $
        let s = "--\n**\n__\n"
         in s ~-> errFancy 3 (nonFlanking "**")
      it "CM17" $
        " ***\n  ***\n   ***" ==-> "<hr>\n<hr>\n<hr>\n"
      it "CM18" $
        "    ***" ==-> "<pre><code>***\n</code></pre>\n"
      it "CM19" $
        let s = "Foo\n    ***\n"
         in s ~-> errFancy 8 (nonFlanking "***")
      it "CM20" $
        "_____________________________________"
          ==-> "<hr>\n"
      it "CM21" $
        " - - -" ==-> "<hr>\n"
      it "CM22" $
        " **  * ** * ** * **" ==-> "<hr>\n"
      it "CM23" $
        "-     -      -      -" ==-> "<hr>\n"
      it "CM24" $
        "- - - -    " ==-> "<hr>\n"
      it "CM25" $
        let s = "_ _ _ _ a\n\na------\n\n---a---\n"
         in s ~-> errFancy 0 (nonFlanking "_")
      it "CM26" $
        " *-*" ==-> "<p><em>-</em></p>\n"
      it "CM27" $
        "- foo\n***\n- bar"
          ==-> "<ul>\n<li>\nfoo\n</li>\n</ul>\n<hr>\n<ul>\n<li>\nbar\n</li>\n</ul>\n"
      it "CM28" $
        "Foo\n***\nbar"
          ==-> "<p>Foo</p>\n<hr>\n<p>bar</p>\n"
      it "CM29" $
        "Foo\n---\nbar"
          ==-> "<p>Foo</p>\n<hr>\n<p>bar</p>\n"
      it "CM30" $
        "* Foo\n* * *\n* Bar"
          ==-> "<ul>\n<li>\nFoo\n</li>\n<li>\n<ul>\n<li>\n<ul>\n<li>\n\n</li>\n</ul>\n</li>\n</ul>\n</li>\n<li>\nBar\n</li>\n</ul>\n"
      it "CM31" $
        "- Foo\n- * * *"
          ==-> "<ul>\n<li>\nFoo\n</li>\n<li>\n<hr>\n</li>\n</ul>\n"
    context "4.2 ATX headings" $ do
      it "CM32" $
        "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo"
          ==-> "<h1 id=\"foo\">foo</h1>\n<h2 id=\"foo\">foo</h2>\n<h3 id=\"foo\">foo</h3>\n<h4 id=\"foo\">foo</h4>\n<h5 id=\"foo\">foo</h5>\n<h6 id=\"foo\">foo</h6>\n"
      it "CM33" $
        let s = "####### foo"
         in s ~-> err 6 (utok '#' <> ews)
      it "CM34" $
        let s = "#5 bolt\n\n#hashtag"
         in s
              ~~-> [ err 1 (utok '5' <> etok '#' <> ews),
                     err 10 (utok 'h' <> etok '#' <> ews)
                   ]
      it "CM35" $
        "\\## foo" ==-> "<p>## foo</p>\n"
      it "CM36" $
        "# foo *bar* \\*baz\\*" ==-> "<h1 id=\"foo-bar-baz\">foo <em>bar</em> *baz*</h1>\n"
      it "CM37" $
        "#                  foo                     "
          ==-> "<h1 id=\"foo\">foo</h1>\n"
      it "CM38" $
        " ### foo\n  ## foo\n   # foo"
          ==-> "<h3 id=\"foo\">foo</h3>\n<h2 id=\"foo\">foo</h2>\n<h1 id=\"foo\">foo</h1>\n"
      it "CM39" $
        "    # foo" ==-> "<pre><code># foo\n</code></pre>\n"
      it "CM40" $
        "foo\n    # bar" ==-> "<p>foo\n# bar</p>\n"
      it "CM41" $
        "## foo ##\n  ###   bar    ###"
          ==-> "<h2 id=\"foo\">foo</h2>\n<h3 id=\"bar\">bar</h3>\n"
      it "CM42" $
        "# foo ##################################\n##### foo ##"
          ==-> "<h1 id=\"foo\">foo</h1>\n<h5 id=\"foo\">foo</h5>\n"
      it "CM43" $
        "### foo ###     " ==-> "<h3 id=\"foo\">foo</h3>\n"
      it "CM44" $
        "### foo ### b" ==-> "<h3 id=\"foo-b\">foo ### b</h3>\n"
      it "CM45" $
        "# foo#" ==-> "<h1 id=\"foo\">foo#</h1>\n"
      it "CM46" $
        "### foo \\###\n## foo #\\##\n# foo \\#"
          ==-> "<h3 id=\"foo\">foo ###</h3>\n<h2 id=\"foo\">foo ###</h2>\n<h1 id=\"foo\">foo #</h1>\n"
      it "CM47" $
        "****\n## foo\n****"
          ==-> "<hr>\n<h2 id=\"foo\">foo</h2>\n<hr>\n"
      it "CM48" $
        "Foo bar\n# baz\nBar foo"
          ==-> "<p>Foo bar</p>\n<h1 id=\"baz\">baz</h1>\n<p>Bar foo</p>\n"
      it "CM49" $
        let s = "## \n#\n### ###"
         in s
              ~~-> [ err 3 (utok '\n' <> elabel "heading character" <> ews),
                     err 5 (utok '\n' <> etok '#' <> ews)
                   ]
    context "4.3 Setext headings" $ do
      -- NOTE we do not support them, the tests have been adjusted
      -- accordingly.
      it "CM50" $
        "Foo *bar*\n=========\n\nFoo *bar*\n---------"
          ==-> "<p>Foo <em>bar</em>\n=========</p>\n<p>Foo <em>bar</em></p>\n<hr>\n"
      it "CM51" $
        "Foo *bar\nbaz*\n===="
          ==-> "<p>Foo <em>bar\nbaz</em>\n====</p>\n"
      it "CM52" $
        "Foo\n-------------------------\n\nFoo\n="
          ==-> "<p>Foo</p>\n<hr>\n<p>Foo\n=</p>\n"
      it "CM53" $
        "   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ==="
          ==-> "<p>Foo</p>\n<hr>\n<p>Foo</p>\n<hr>\n<p>Foo\n===</p>\n"
      it "CM54" $
        "    Foo\n    ---\n\n    Foo\n---"
          ==-> "<pre><code>Foo\n---\n\nFoo\n</code></pre>\n<hr>\n"
      it "CM55" $
        "Foo\n   ----      "
          ==-> "<p>Foo</p>\n<hr>\n"
      it "CM56" $
        "Foo\n    ---"
          ==-> "<p>Foo\n---</p>\n"
      it "CM57" $
        "Foo\n= =\n\nFoo\n--- -"
          ==-> "<p>Foo\n= =</p>\n<p>Foo</p>\n<hr>\n"
      it "CM58" $
        "Foo  \n-----"
          ==-> "<p>Foo</p>\n<hr>\n"
      it "CM59" $
        "Foo\\\n----"
          ==-> "<p>Foo\\</p>\n<hr>\n"
      it "CM60" $
        let s = "`Foo\n----\n`\n\n<a title=\"a lot\n---\nof dashes\"/>\n"
         in s
              ~~-> [ err 4 (ueib <> etok '`' <> ecsc),
                     err 11 (ueib <> etok '`' <> ecsc)
                   ]
      it "CM61" $
        "> Foo\n---"
          ==-> "<blockquote>\n<p>Foo</p>\n</blockquote>\n<hr>\n"
      it "CM62" $
        "> foo\nbar\n==="
          ==-> "<blockquote>\n<p>foo</p>\n</blockquote>\n<p>bar\n===</p>\n"
      it "CM63" $
        "- Foo\n---"
          ==-> "<ul>\n<li>\nFoo\n</li>\n</ul>\n<hr>\n"
      it "CM64" $
        "Foo\nBar\n---"
          ==-> "<p>Foo\nBar</p>\n<hr>\n"
      it "CM65" $
        "---\nFoo\n---\nBar\n---\nBaz"
          ==-> "<p>Bar</p>\n<hr>\n<p>Baz</p>\n"
      it "CM66" $
        "\n===="
          ==-> "<p>====</p>\n"
      it "CM67" $
        "---\n---"
          ==-> "" -- thinks that it's got a YAML block
      it "CM68" $
        "- foo\n-----"
          ==-> "<ul>\n<li>\nfoo\n</li>\n</ul>\n<hr>\n"
      it "CM69" $
        "    foo\n---"
          ==-> "<pre><code>foo\n</code></pre>\n<hr>\n"
      it "CM70" $
        "> foo\n-----"
          ==-> "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr>\n"
      it "CM71" $
        "\\> foo\n------"
          ==-> "<p>&gt; foo</p>\n<hr>\n"
      it "CM72" $
        "Foo\n\nbar\n---\nbaz"
          ==-> "<p>Foo</p>\n<p>bar</p>\n<hr>\n<p>baz</p>\n"
      it "CM73" $
        "Foo\nbar\n\n---\n\nbaz"
          ==-> "<p>Foo\nbar</p>\n<hr>\n<p>baz</p>\n"
      it "CM74" $
        "Foo\nbar\n* * *\nbaz"
          ==-> "<p>Foo\nbar</p>\n<hr>\n<p>baz</p>\n"
      it "CM75" $
        "Foo\nbar\n\\---\nbaz"
          ==-> "<p>Foo\nbar\n---\nbaz</p>\n"
    context "4.4 Indented code blocks" $ do
      it "CM76" $
        "    a simple\n      indented code block"
          ==-> "<pre><code>a simple\n  indented code block\n</code></pre>\n"
      it "CM77" $
        "  - foo\n\n    bar"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      it "CM78" $
        "1.  foo\n\n    - bar"
          ==-> "<ol>\n<li>\n<p>foo</p>\n<ul>\n<li>\nbar\n</li>\n</ul>\n</li>\n</ol>\n"
      it "CM79" $
        "    <a/>\n    *hi*\n\n    - one"
          ==-> "<pre><code>&lt;a/&gt;\n*hi*\n\n- one\n</code></pre>\n"
      it "CM80" $
        "    chunk1\n\n    chunk2\n  \n \n \n    chunk3"
          ==-> "<pre><code>chunk1\n\nchunk2\n\n\n\nchunk3\n</code></pre>\n"
      it "CM81" $
        "    chunk1\n      \n      chunk2"
          ==-> "<pre><code>chunk1\n  \n  chunk2\n</code></pre>\n"
      it "CM82" $
        "Foo\n    bar\n"
          ==-> "<p>Foo\nbar</p>\n"
      it "CM83" $
        "    foo\nbar"
          ==-> "<pre><code>foo\n</code></pre>\n<p>bar</p>\n"
      it "CM84" $
        "# Heading\n    foo\nHeading\n------\n    foo\n----\n"
          ==-> "<h1 id=\"heading\">Heading</h1>\n<pre><code>foo\n</code></pre>\n<p>Heading</p>\n<hr>\n<pre><code>foo\n</code></pre>\n<hr>\n"
      it "CM85" $
        "        foo\n    bar"
          ==-> "<pre><code>    foo\nbar\n</code></pre>\n"
      it "CM86" $
        "\n    \n    foo\n    \n"
          ==-> "<pre><code>foo\n</code></pre>\n"
      it "CM87" $
        "    foo  "
          ==-> "<pre><code>foo  \n</code></pre>\n"
    context "4.5 Fenced code blocks" $ do
      it "CM88" $
        "```\n<\n >\n```"
          ==-> "<pre><code>&lt;\n &gt;\n</code></pre>\n"
      it "CM89" $
        "~~~\n<\n >\n~~~"
          ==-> "<pre><code>&lt;\n &gt;\n</code></pre>\n"
      it "CM90" $
        "``\nfoo\n``\n"
          ==-> "<p><code>foo</code></p>\n"
      it "CM91" $
        "```\naaa\n~~~\n```"
          ==-> "<pre><code>aaa\n~~~\n</code></pre>\n"
      it "CM92" $
        "~~~\naaa\n```\n~~~"
          ==-> "<pre><code>aaa\n```\n</code></pre>\n"
      it "CM93" $
        "````\naaa\n```\n``````"
          ==-> "<pre><code>aaa\n```\n</code></pre>\n"
      it "CM94" $
        "~~~~\naaa\n~~~\n~~~~"
          ==-> "<pre><code>aaa\n~~~\n</code></pre>\n"
      it "CM95" $
        let s = "```"
         in s ~-> err 3 (ueib <> etok '`' <> ecsc)
      it "CM96" $
        let s = "`````\n\n```\naaa\n"
         in s
              ~-> err
                15
                (ueof <> elabel "closing code fence" <> elabel "code block content")
      it "CM97" $
        let s = "> ```\n> aaa\n\nbbb\n"
         in s ~-> err 17 (ueof <> elabel "closing code fence" <> elabel "code block content")
      it "CM98" $
        "```\n\n  \n```"
          ==-> "<pre><code>\n  \n</code></pre>\n"
      it "CM99" $
        "```\n```"
          ==-> "<pre><code></code></pre>\n"
      it "CM100" $
        " ```\n aaa\naaa\n```"
          ==-> "<pre><code>aaa\naaa\n</code></pre>\n"
      it "CM101" $
        "  ```\naaa\n  aaa\naaa\n  ```"
          ==-> "<pre><code>aaa\naaa\naaa\n</code></pre>\n"
      it "CM102" $
        "   ```\n   aaa\n    aaa\n  aaa\n   ```"
          ==-> "<pre><code>aaa\n aaa\naaa\n</code></pre>\n"
      it "CM103" $
        "    ```\n    aaa\n    ```"
          ==-> "<pre><code>```\naaa\n```\n</code></pre>\n"
      it "CM104" $
        "```\naaa\n  ```"
          ==-> "<pre><code>aaa\n</code></pre>\n"
      it "CM105" $
        "   ```\naaa\n  ```"
          ==-> "<pre><code>aaa\n</code></pre>\n"
      it "CM106" $
        let s = "```\naaa\n    ```\n"
         in s
              ~-> err
                16
                (ueof <> elabel "closing code fence" <> elabel "code block content")
      it "CM107" $
        "``` ```\naaa"
          ==-> "<p><code></code>\naaa</p>\n"
      it "CM108" $
        let s = "~~~~~~\naaa\n~~~ ~~\n"
         in s
              ~-> err
                18
                (ueof <> elabel "closing code fence" <> elabel "code block content")
      it "CM109" $
        "foo\n```\nbar\n```\nbaz"
          ==-> "<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n"
      it "CM110" $
        "foo\n---\n~~~\nbar\n~~~\n# baz"
          ==-> "<p>foo</p>\n<hr>\n<pre><code>bar\n</code></pre>\n<h1 id=\"baz\">baz</h1>\n"
      it "CM111" $
        "```ruby\ndef foo(x)\n  return 3\nend\n```"
          ==-> "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
      it "CM112" $
        "~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~"
          ==-> "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
      it "CM113" $
        "````;\n````"
          ==-> "<pre><code class=\"language-;\"></code></pre>\n"
      it "CM114" $
        "``` aa ```\nfoo"
          ==-> "<p><code>aa</code>\nfoo</p>\n"
      it "CM115" $
        "```\n``` aaa\n```"
          ==-> "<pre><code>``` aaa\n</code></pre>\n"
    context "4.6 HTML blocks" $
      -- NOTE We do not support HTML blocks, see the readme.
      return ()
    context "4.7 Link reference definitions" $ do
      it "CM159" $
        "[foo]: /url \"title\"\n\n[foo]" ##-> p_ (a_ [href_ "/url", title_ "title"] "foo")
      it "CM160" $
        "   [foo]: \n      /url  \n           'the title'  \n\n[foo]"
          ##-> p_ (a_ [href_ "/url", title_ "the title"] "foo")
      it "CM161" $
        let s = "[Foo bar\\]]:my_(url) 'title (with parens)'\n\n[Foo bar\\]]"
         in s
              ~~-> [ err 19 (utoks ") " <> euric <> elabel "newline" <> ews),
                     errFancy 45 (couldNotMatchRef "Foo bar]" [])
                   ]
      it "CM162" $
        "[Foo bar]:\n<my%20url>\n'title'\n\n[Foo bar]"
          ##-> p_ (a_ [href_ "my%20url", title_ "title"] "Foo bar")
      it "CM163" $
        "[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]"
          ##-> p_ (a_ [href_ "/url", title_ "\ntitle\nline1\nline2\n"] "foo")
      it "CM164" $
        "[foo]: /url 'title\n\nwith blank line'\n\n[foo]"
          ##-> p_ (a_ [href_ "/url", title_ "title\n\nwith blank line"] "foo")
      it "CM165" $
        "[foo]:\n/url\n\n[foo]"
          ==-> "<p><a href=\"/url\">foo</a></p>\n"
      it "CM166" $
        let s = "[foo]:\n\n[foo]"
         in s
              ~~-> [ err 7 (utok '\n' <> etok '<' <> elabel "URI" <> ews),
                     errFancy 9 (couldNotMatchRef "foo" [])
                   ]
      it "CM167" $
        let s = "[foo]: /url\\bar\\*baz \"foo\\\"bar\\baz\"\n\n[foo]\n"
         in s ~-> err 11 (utok '\\' <> euric <> euri)
      it "CM168" $
        "[foo]\n\n[foo]: url"
          ==-> "<p><a href=\"url\">foo</a></p>\n"
      it "CM169" $
        let s = "[foo]\n\n[foo]: first\n[foo]: second\n"
         in s ~-> errFancy 21 (duplicateRef "foo")
      it "CM170" $
        "[FOO]: /url\n\n[Foo]"
          ==-> "<p><a href=\"/url\">Foo</a></p>\n"
      it "CM171" $
        "[ΑΓΩ]: /%CF%86%CE%BF%CF%85\n\n[αγω]"
          ==-> "<p><a href=\"/%cf%86%ce%bf%cf%85\">αγω</a></p>\n"
      it "CM172" $
        "[foo]: /url"
          ==-> ""
      it "CM173" $
        "[\nfoo\n]: /url\nbar"
          ==-> "<p>bar</p>\n"
      it "CM174" $
        let s = "[foo]: /url \"title\" ok"
         in s ~-> err 20 (utoks "ok" <> elabel "newline" <> ews)
      it "CM175" $
        let s = "[foo]: /url\n\"title\" ok\n"
         in s ~-> err 20 (utoks "ok" <> elabel "newline" <> ews)
      it "CM176" $
        "    [foo]: /url \"title\""
          ==-> "<pre><code>[foo]: /url &quot;title&quot;\n</code></pre>\n"
      it "CM177" $
        "```\n[foo]: /url\n```"
          ==-> "<pre><code>[foo]: /url\n</code></pre>\n"
      it "CM178" $
        let s = "Foo\n[bar]: /baz\n\n[bar]\n"
         in s
              ~~-> [ errFancy 5 (couldNotMatchRef "bar" []),
                     errFancy 18 (couldNotMatchRef "bar" [])
                   ]
      it "CM179" $
        "# [Foo]\n[foo]: /url\n> bar"
          ==-> "<h1 id=\"foo\"><a href=\"/url\">Foo</a></h1>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
      it "CM180" $
        "[foo]: /foo-url \"foo\"\n[bar]: /bar-url\n  \"bar\"\n[baz]: /baz-url\n\n[foo],\n[bar],\n[baz]"
          ##-> p_
            ( do
                a_ [href_ "/foo-url", title_ "foo"] "foo"
                ",\n"
                a_ [href_ "/bar-url", title_ "bar"] "bar"
                ",\n"
                a_ [href_ "/baz-url"] "baz"
            )
      it "CM181" $
        "[foo]\n\n> [foo]: /url"
          ==-> "<p><a href=\"/url\">foo</a></p>\n<blockquote>\n</blockquote>\n"
    context "4.8 Paragraphs" $ do
      it "CM182" $
        "aaa\n\nbbb"
          ==-> "<p>aaa</p>\n<p>bbb</p>\n"
      it "CM183" $
        "aaa\nbbb\n\nccc\nddd"
          ==-> "<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n"
      it "CM184" $
        "aaa\n\n\nbbb"
          ==-> "<p>aaa</p>\n<p>bbb</p>\n"
      it "CM185" $
        "  aaa\n bbb"
          ==-> "<p>aaa\nbbb</p>\n"
      it "CM186" $
        "aaa\n             bbb\n                                       ccc"
          ==-> "<p>aaa\nbbb\nccc</p>\n"
      it "CM187" $
        "   aaa\nbbb" ==-> "<p>aaa\nbbb</p>\n"
      it "CM188" $
        "    aaa\nbbb"
          ==-> "<pre><code>aaa\n</code></pre>\n<p>bbb</p>\n"
      it "CM189" $
        "aaa     \nbbb     "
          ==-> "<p>aaa\nbbb</p>\n"
    context "4.9 Blank lines" $
      it "CM190" $
        "  \n\naaa\n  \n\n# aaa\n\n  "
          ==-> "<p>aaa</p>\n<h1 id=\"aaa\">aaa</h1>\n"
    context "5.1 Block quotes" $ do
      it "CM191" $
        "> # Foo\n  bar\n  baz"
          ==-> "<blockquote>\n<h1 id=\"foo\">Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM192" $
        "># Foo\n bar\n  baz"
          ==-> "<blockquote>\n<h1 id=\"foo\">Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM193" $
        "   > # Foo\n     bar\n     baz"
          ==-> "<blockquote>\n<h1 id=\"foo\">Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM194" $
        "    > # Foo\n    > bar\n    > baz"
          ==-> "<pre><code>&gt; # Foo\n&gt; bar\n&gt; baz\n</code></pre>\n"
      it "CM195" $
        "> # Foo\n> bar\nbaz"
          ==-> "<blockquote>\n<h1 id=\"foo\">Foo</h1>\n</blockquote>\n<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
      it "CM196" $
        "> bar\nbaz\n> foo"
          ==-> "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n<blockquote>\n<p>foo</p>\n</blockquote>\n"
      it "CM197" $
        "> foo\n---"
          ==-> "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr>\n"
      it "CM198" $
        "> - foo\n- bar"
          ==-> "<blockquote>\n<ul>\n<li>\nfoo\n</li>\n</ul>\n</blockquote>\n<ul>\n<li>\nbar\n</li>\n</ul>\n"
      it "CM199" $
        ">     foo\n    bar"
          ==-> "<blockquote>\n<pre><code>foo\n</code></pre>\n<p>bar</p>\n</blockquote>\n"
      it "CM200" $
        "> ```\nfoo\n```"
          ==-> "<blockquote>\n<pre><code>foo\n</code></pre>\n</blockquote>\n"
      it "CM201" $
        "> foo\n    - bar"
          ==-> "<blockquote>\n<p>foo</p>\n<ul>\n<li>\nbar\n</li>\n</ul>\n</blockquote>\n"
      it "CM202" $
        ">"
          ==-> "<blockquote>\n</blockquote>\n"
      it "CM203" $
        ">\n>  \n> "
          ==-> "<blockquote>\n</blockquote>\n<blockquote>\n</blockquote>\n<blockquote>\n</blockquote>\n"
      it "CM204" $
        ">\n  foo\n   "
          ==-> "<blockquote>\n<p>foo</p>\n</blockquote>\n"
      it "CM205" $
        "> foo\n\n> bar"
          ==-> "<blockquote>\n<p>foo</p>\n</blockquote>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
      it "CM206" $
        "> foo\n  bar"
          ==-> "<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n"
      it "CM207" $
        "> foo\n\n  bar"
          ==-> "<blockquote>\n<p>foo</p>\n<p>bar</p>\n</blockquote>\n"
      it "CM208" $
        "foo\n> bar"
          ==-> "<p>foo</p>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
      it "CM209" $
        "> aaa\n***\n> bbb"
          ==-> "<blockquote>\n<p>aaa</p>\n</blockquote>\n<hr>\n<blockquote>\n<p>bbb</p>\n</blockquote>\n"
      it "CM210" $
        "> bar\n  baz"
          ==-> "<blockquote>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM211" $
        "> bar\n\nbaz"
          ==-> "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
      it "CM212" $
        "> bar\n\nbaz"
          ==-> "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
      it "CM213" $
        "> > > foo\nbar"
          ==-> "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo</p>\n</blockquote>\n</blockquote>\n</blockquote>\n<p>bar</p>\n"
      it "CM214" $
        ">>> foo\n    bar\n    baz"
          ==-> "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar\nbaz</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"
      it "CM215" $
        ">     code\n\n>    not code"
          ==-> "<blockquote>\n<pre><code>code\n</code></pre>\n</blockquote>\n<blockquote>\n<p>not code</p>\n</blockquote>\n"
    context "5.2 List items" $ do
      it "CM216" $
        "A paragraph\nwith two lines.\n\n    indented code\n\n> A block quote."
          ==-> "<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n"
      it "CM217" $
        "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote."
          ==-> "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM218" $
        "- one\n\n two"
          ==-> "<ul>\n<li>\none\n</li>\n</ul>\n<p>two</p>\n"
      it "CM219" $
        "- one\n\n  two"
          ==-> "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"
      it "CM220" $
        " -    one\n\n     two"
          ==-> "<ul>\n<li>\none\n</li>\n</ul>\n<pre><code> two\n</code></pre>\n"
      it "CM221" $
        " -    one\n\n      two"
          ==-> "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"
      it "CM222" $
        "   > > 1.  one\n\n       two"
          ==-> "<blockquote>\n<blockquote>\n<ol>\n<li>\none\n</li>\n</ol>\n<p>two</p>\n</blockquote>\n</blockquote>\n"
      it "CM223" $
        ">>- one\n\n     two"
          ==-> "<blockquote>\n<blockquote>\n<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n</blockquote>\n</blockquote>\n"
      it "CM224" $
        "-one\n\n2.two"
          ==-> "<p>-one</p>\n<p>2.two</p>\n"
      it "CM225" $
        "- foo\n\n\n  bar"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      it "CM226" $
        "1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam"
          ==-> "<ol>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n<blockquote>\n<p>bam</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM227" $
        "- Foo\n\n      bar\n\n\n      baz"
          ==-> "<ul>\n<li>\n<p>Foo</p>\n<pre><code>bar\n\n\nbaz\n</code></pre>\n</li>\n</ul>\n"
      it "CM228" $
        "123456789. ok"
          ==-> "<ol start=\"123456789\">\n<li>\nok\n</li>\n</ol>\n"
      it "CM229" $
        let s = "1234567890. not ok\n"
         in s ~-> errFancy 0 (indexTooBig 1234567890)
      it "CM230" $
        "0. ok"
          ==-> "<ol start=\"0\">\n<li>\nok\n</li>\n</ol>\n"
      it "CM231" $
        "003. ok"
          ==-> "<ol start=\"3\">\n<li>\nok\n</li>\n</ol>\n"
      it "CM232" $
        "-1. not ok"
          ==-> "<p>-1. not ok</p>\n"
      it "CM233" $
        "- foo\n\n      bar"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ul>\n"
      it "CM234" $
        "  10.  foo\n\n           bar"
          ==-> "<ol start=\"10\">\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ol>\n"
      it "CM235" $
        "    indented code\n\nparagraph\n\n    more code"
          ==-> "<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n"
      it "CM236" $
        "1.     indented code\n\n   paragraph\n\n       more code"
          ==-> "<ol>\n<li>\n<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>\n"
      it "CM237" $
        "1.      indented code\n\n   paragraph\n\n       more code"
          ==-> "<ol>\n<li>\n<pre><code> indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>\n"
      it "CM238" $
        "   foo\n\nbar"
          ==-> "<p>foo</p>\n<p>bar</p>\n"
      it "CM239" $
        "-    foo\n\n  bar"
          ==-> "<ul>\n<li>\nfoo\n</li>\n</ul>\n<p>bar</p>\n"
      it "CM240" $
        "-  foo\n\n   bar"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      it "CM241" $
        "-\n  foo\n-\n  ```\n  bar\n  ```\n-\n      baz"
          ==-> "<ul>\n<li>\n<p>foo</p>\n</li>\n<li>\n<pre><code>bar\n</code></pre>\n</li>\n<li>\n<pre><code>baz\n</code></pre>\n</li>\n</ul>\n"
      it "CM242" $
        "-   \n  foo"
          ==-> "<ul>\n<li>\nfoo\n</li>\n</ul>\n"
      it "CM243a" $
        "-\n\n  foo"
          ==-> "<ul>\n<li>\n\n</li>\n</ul>\n<p>foo</p>\n"
      it "CM243b" $
        "1.\n\n   foo"
          ==-> "<ol>\n<li>\n\n</li>\n</ol>\n<p>foo</p>\n"
      it "CM244" $
        "- foo\n-\n- bar"
          ==-> "<ul>\n<li>\nfoo\n</li>\n<li>\n\n</li>\n<li>\nbar\n</li>\n</ul>\n"
      it "CM245" $
        "- foo\n-   \n- bar"
          ==-> "<ul>\n<li>\nfoo\n</li>\n<li>\n\n</li>\n<li>\nbar\n</li>\n</ul>\n"
      it "CM246" $
        "1. foo\n2.\n3. bar"
          ==-> "<ol>\n<li>\nfoo\n</li>\n<li>\n\n</li>\n<li>\nbar\n</li>\n</ol>\n"
      it "CM247" $
        "*"
          ==-> "<ul>\n<li>\n\n</li>\n</ul>\n"
      it "CM248" $
        "foo\n*\n\nfoo\n1."
          ==-> "<p>foo</p>\n<ul>\n<li>\n\n</li>\n</ul>\n<p>foo</p>\n<ol>\n<li>\n\n</li>\n</ol>\n"
      it "CM249" $
        " 1.  A paragraph\n     with two lines.\n\n         indented code\n\n     > A block quote."
          ==-> "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM250" $
        "  1.  A paragraph\n      with two lines.\n\n          indented code\n\n      > A block quote."
          ==-> "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM251" $
        "   1.  A paragraph\n       with two lines.\n\n           indented code\n\n       > A block quote."
          ==-> "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM252" $
        "    1.  A paragraph\n        with two lines.\n\n            indented code\n\n        > A block quote."
          ==-> "<pre><code>1.  A paragraph\n    with two lines.\n\n        indented code\n\n    &gt; A block quote.\n</code></pre>\n"
      it "CM253" $
        "  1.  A paragraph\nwith two lines.\n\n          indented code\n\n      > A block quote."
          ==-> "<ol>\n<li>\nA paragraph\n</li>\n</ol>\n<p>with two lines.</p>\n<pre><code>      indented code\n\n  &gt; A block quote.\n</code></pre>\n"
      it "CM254" $
        "  1.  A paragraph\n    with two lines."
          ==-> "<ol>\n<li>\nA paragraph\n</li>\n</ol>\n<pre><code>with two lines.\n</code></pre>\n"
      it "CM255" $
        "> 1. > Blockquote\ncontinued here."
          ==-> "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>\n<p>continued here.</p>\n"
      it "CM256" $
        "> 1. > Blockquote\n  continued here."
          ==-> "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote</p>\n</blockquote>\n</li>\n</ol>\n<p>continued here.</p>\n</blockquote>\n"
      it "CM257" $
        "- foo\n  - bar\n    - baz\n      - boo"
          ==-> "<ul>\n<li>\nfoo\n<ul>\n<li>\nbar\n<ul>\n<li>\nbaz\n<ul>\n<li>\nboo\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM258" $
        "- foo\n - bar\n  - baz\n   - boo"
          ==-> "<ul>\n<li>\nfoo\n</li>\n<li>\nbar\n</li>\n<li>\nbaz\n</li>\n<li>\nboo\n</li>\n</ul>\n"
      it "CM259" $
        "10) foo\n    - bar"
          ==-> "<ol start=\"10\">\n<li>\nfoo\n<ul>\n<li>\nbar\n</li>\n</ul>\n</li>\n</ol>\n"
      it "CM260" $
        "10) foo\n   - bar"
          ==-> "<ol start=\"10\">\n<li>\nfoo\n</li>\n</ol>\n<ul>\n<li>\nbar\n</li>\n</ul>\n"
      it "CM261" $
        "- - foo"
          ==-> "<ul>\n<li>\n<ul>\n<li>\nfoo\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM262" $
        "1. - 2. foo"
          ==-> "<ol>\n<li>\n<ul>\n<li>\n<ol start=\"2\">\n<li>\nfoo\n</li>\n</ol>\n</li>\n</ul>\n</li>\n</ol>\n"
      it "CM263" $
        "- # Foo\n- Bar\n  ---\n  baz"
          ==-> "<ul>\n<li>\n<h1 id=\"foo\">Foo</h1>\n</li>\n<li>\n<p>Bar</p>\n<hr>\n<p>baz</p>\n</li>\n</ul>\n"
    context "5.3 Lists" $ do
      it "CM264" $
        "- foo\n- bar\n+ baz"
          ==-> "<ul>\n<li>\nfoo\n</li>\n<li>\nbar\n</li>\n</ul>\n<ul>\n<li>\nbaz\n</li>\n</ul>\n"
      it "CM265" $
        "1. foo\n2. bar\n3) baz"
          ==-> "<ol>\n<li>\nfoo\n</li>\n<li>\nbar\n</li>\n</ol>\n<ol start=\"3\">\n<li>\nbaz\n</li>\n</ol>\n"
      it "CM266" $
        "Foo\n- bar\n- baz"
          ==-> "<p>Foo</p>\n<ul>\n<li>\nbar\n</li>\n<li>\nbaz\n</li>\n</ul>\n"
      it "CM267" $
        "The number of windows in my house is\n14.  The number of doors is 6."
          ==-> "<p>The number of windows in my house is</p>\n<ol start=\"14\">\n<li>\nThe number of doors is 6.\n</li>\n</ol>\n"
      it "CM268" $
        "The number of windows in my house is\n1.  The number of doors is 6."
          ==-> "<p>The number of windows in my house is</p>\n<ol>\n<li>\nThe number of doors is 6.\n</li>\n</ol>\n"
      it "CM269" $
        "- foo\n\n- bar\n\n\n- baz"
          ==-> "<ul>\n<li>\n<p>foo</p>\n</li>\n<li>\n<p>bar</p>\n</li>\n<li>\n<p>baz</p>\n</li>\n</ul>\n"
      it "CM270" $
        "- foo\n  - bar\n    - baz\n\n\n      bim"
          ==-> "<ul>\n<li>\nfoo\n<ul>\n<li>\nbar\n<ul>\n<li>\n<p>baz</p>\n<p>bim</p>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM271" $
        "- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim"
          ==-> "<ul>\n<li>\nfoo\n</li>\n<li>\nbar\n</li>\n</ul>\n<p>&lt;!-- --&gt;</p>\n<ul>\n<li>\nbaz\n</li>\n<li>\nbim\n</li>\n</ul>\n"
      it "CM272" $
        "-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<p>notcode</p>\n</li>\n<li>\n<p>foo</p>\n</li>\n</ul>\n<p>&lt;!-- --&gt;</p>\n<pre><code>code\n</code></pre>\n"
      it "CM273" $
        "- a\n - b\n  - c\n   - d\n    - e\n   - f\n  - g\n - h\n- i"
          ==-> "<ul>\n<li>\na\n</li>\n<li>\nb\n</li>\n<li>\nc\n</li>\n<li>\nd\n</li>\n<li>\ne\n</li>\n<li>\nf\n</li>\n<li>\ng\n</li>\n<li>\nh\n</li>\n<li>\ni\n</li>\n</ul>\n"
      it "CM274" $
        "1. a\n\n  2. b\n\n    3. c"
          ==-> "<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ol>\n"
      it "CM275" $
        "- a\n- b\n\n- c"
          ==-> "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
      it "CM276" $
        "* a\n*\n\n* c"
          ==-> "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p></p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
      it "CM277" $
        "- a\n- b\n\n  c\n- d"
          ==-> "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
      it "CM278" $
        "- a\n- b\n\n  [ref]: /url\n- d"
          ==-> "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
      it "CM279" $
        "- a\n- ```\n  b\n\n\n  ```\n- c"
          ==-> "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<pre><code>b\n\n\n</code></pre>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
      it "CM280" $
        "- a\n  - b\n\n    c\n- d"
          ==-> "<ul>\n<li>\na\n<ul>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n</ul>\n</li>\n<li>\nd\n</li>\n</ul>\n"
      it "CM281" $
        "* a\n  > b\n  >\n* c"
          ==-> "<ul>\n<li>\n<p>a</p>\n<blockquote>\n<p>b</p>\n</blockquote>\n<blockquote>\n</blockquote>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
      it "CM282" $
        "- a\n  > b\n  ```\n  c\n  ```\n- d"
          ==-> "<ul>\n<li>\n<p>a</p>\n<blockquote>\n<p>b</p>\n</blockquote>\n<pre><code>c\n</code></pre>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
      it "CM283" $
        "- a"
          ==-> "<ul>\n<li>\na\n</li>\n</ul>\n"
      it "CM284" $
        "- a\n  - b"
          ==-> "<ul>\n<li>\na\n<ul>\n<li>\nb\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM285" $
        "1. ```\n   foo\n   ```\n\n   bar"
          ==-> "<ol>\n<li>\n<pre><code>foo\n</code></pre>\n<p>bar</p>\n</li>\n</ol>\n"
      it "CM286" $
        "* foo\n  * bar\n\n  baz"
          ==-> "<ul>\n<li>\nfoo\n<ul>\n<li>\nbar\n</li>\n</ul>\nbaz\n</li>\n</ul>\n"
      it "CM287" $
        "- a\n  - b\n  - c\n\n- d\n  - e\n  - f"
          ==-> "<ul>\n<li>\na\n<ul>\n<li>\nb\n</li>\n<li>\nc\n</li>\n</ul>\n</li>\n<li>\nd\n<ul>\n<li>\ne\n</li>\n<li>\nf\n</li>\n</ul>\n</li>\n</ul>\n"
    context "6 Inlines" $
      it "CM288" $
        let s = "`hi`lo`\n"
         in s ~-> err 7 (ueib <> etok '`' <> ecsc)
    context "6.1 Blackslash escapes" $ do
      it "CM289" $
        "\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~\n"
          ==-> "<p>!&quot;#$%&amp;&#39;()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>\n"
      it "CM290" $
        "\\\t\\A\\a\\ \\3\\φ\\«"
          ==-> "<p>\\\t\\A\\a\\ \\3\\φ\\«</p>\n"
      it "CM291" $
        "\\*not emphasized\\*\n\\<br/> not a tag\n\\[not a link\\](/foo)\n\\`not code\\`\n1\\. not a list\n\\* not a list\n\\# not a heading\n\\[foo\\]: /url \"not a reference\"\n"
          ==-> "<p>*not emphasized*\n&lt;br/&gt; not a tag\n[not a link](/foo)\n`not code`\n1. not a list\n* not a list\n# not a heading\n[foo]: /url &quot;not a reference&quot;</p>\n"
      it "CM292" $
        let s = "\\\\*emphasis*"
         in s ~-> errFancy 2 (nonFlanking "*")
      it "CM293" $
        "foo\\\nbar"
          ==-> "<p>foo<br>\nbar</p>\n"
      it "CM294" $
        "`` \\[\\` ``"
          ==-> "<p><code>\\[\\`</code></p>\n"
      it "CM295" $
        "    \\[\\]"
          ==-> "<pre><code>\\[\\]\n</code></pre>\n"
      it "CM296" $
        "~~~\n\\[\\]\n~~~"
          ==-> "<pre><code>\\[\\]\n</code></pre>\n"
      it "CM297" $
        "<http://example.com?find=*>"
          ==-> "<p><a href=\"http://example.com?find=*\">http://example.com?find=*</a></p>\n"
      it "CM298" $
        "<a href=\"/bar\\/)\">"
          ==-> "<p>&lt;a href=&quot;/bar/)&quot;&gt;</p>\n"
      it "CM299" $
        let s = "[foo](/bar\\* \"ti\\*tle\")"
         in s ~-> err 10 (utok '\\' <> euric <> euri)
      it "CM300" $
        let s = "[foo]\n\n[foo]: /bar\\* \"ti\\*tle\""
         in s
              ~~-> [ errFancy 1 (couldNotMatchRef "foo" []),
                     err 18 (utok '\\' <> euric <> euri)
                   ]
      it "CM301" $
        "``` foo\\+bar\nfoo\n```"
          ==-> "<pre><code class=\"language-foo+bar\">foo\n</code></pre>\n"
    context "6.2 Entity and numeric character references" $ do
      it "CM302" $
        "&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;"
          ==-> "<p>  &amp; © Æ Ď\n¾ ℋ ⅆ\n∲ ≧̸</p>\n"
      it "CM303a" $
        "&#35; &#1234; &#992;"
          ==-> "<p># Ӓ Ϡ</p>\n"
      it "CM303b" $
        "&#98765432;" ~-> errFancy 0 (invalidNumChar 98765432)
      it "CM303c" $
        "&#0;" ~-> errFancy 0 (invalidNumChar 0)
      it "CM304" $
        "&#X22; &#XD06; &#xcab;"
          ==-> "<p>&quot; ആ ಫ</p>\n"
      it "CM305a" $
        "&nbsp" ==-> "<p>&amp;nbsp</p>\n"
      it "CM305b" $
        let s = "&x;"
         in s ~-> errFancy 0 (unknownEntity "x")
      it "CM305c" $
        let s = "&#;"
         in s ~-> err 2 (utok ';' <> etok 'x' <> etok 'X' <> elabel "integer")
      it "CM305d" $
        let s = "&#x;"
         in s ~-> err 3 (utok ';' <> elabel "hexadecimal integer")
      it "CM305e" $
        let s = "&ThisIsNotDefined;"
         in s ~-> errFancy 0 (unknownEntity "ThisIsNotDefined")
      it "CM305f" $
        "&hi?;" ==-> "<p>&amp;hi?;</p>\n"
      it "CM306" $
        "&copy"
          ==-> "<p>&amp;copy</p>\n"
      it "CM307" $
        let s = "&MadeUpEntity;"
         in s ~-> errFancy 0 (unknownEntity "MadeUpEntity")
      it "CM308" $
        "<a href=\"&ouml;&ouml;.html\">"
          ==-> "<p>&lt;a href=&quot;\246\246.html&quot;&gt;</p>\n"
      it "CM309" $
        "[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")"
          ##-> p_ (a_ [href_ "/f%26ouml%3b%26ouml%3b", title_ "f\246\246"] "foo")
      it "CM310" $
        "[foo]\n\n[foo]: /f&ouml;&ouml; \"f&ouml;&ouml;\""
          ##-> p_ (a_ [href_ "/f%26ouml%3b%26ouml%3b", title_ "f\246\246"] "foo")
      it "CM311" $
        "``` f&ouml;&ouml;\nfoo\n```"
          ==-> "<pre><code class=\"language-f\246\246\">foo\n</code></pre>\n"
      it "CM312" $
        "`f&ouml;&ouml;`"
          ==-> "<p><code>f&amp;ouml;&amp;ouml;</code></p>\n"
      it "CM313" $
        "    f&ouml;f&ouml;"
          ==-> "<pre><code>f&amp;ouml;f&amp;ouml;\n</code></pre>\n"
    context "6.3 Code spans" $ do
      it "CM314" $
        "`foo`" ==-> "<p><code>foo</code></p>\n"
      it "CM315" $
        "`` foo ` bar  ``"
          ==-> "<p><code>foo ` bar</code></p>\n"
      it "CM316" $
        "` `` `" ==-> "<p><code>``</code></p>\n"
      it "CM317" $
        "``\nfoo\n``" ==-> "<p><code>foo</code></p>\n"
      it "CM318" $
        "`foo   bar\n  baz`" ==-> "<p><code>foo bar baz</code></p>\n"
      it "CM319" $
        "`a  b`" ==-> "<p><code>a  b</code></p>\n"
      it "CM320" $
        "`foo `` bar`" ==-> "<p><code>foo `` bar</code></p>\n"
      it "CM321" $
        let s = "`foo\\`bar`\n"
         in s ~-> err 10 (ueib <> etok '`' <> ecsc)
      it "CM322" $
        let s = "*foo`*`\n"
         in s ~-> err 7 (ueib <> etok '*' <> eic)
      it "CM323" $
        let s = "[not a `link](/foo`)\n"
         in s ~-> err 20 (ueib <> etok ']' <> eic)
      it "CM324" $
        let s = "`<a href=\"`\">`\n"
         in s ~-> err 14 (ueib <> etok '`' <> ecsc)
      it "CM325" $
        "<a href=\"`\">`"
          ==-> "<p>&lt;a href=&quot;<code>&quot;&gt;</code></p>\n"
      it "CM326" $
        let s = "`<http://foo.bar.`baz>`\n"
         in s ~-> err 23 (ueib <> etok '`' <> ecsc)
      it "CM327" $
        "<http://foo.bar.`baz>`"
          ==-> "<p>&lt;http://foo.bar.<code>baz&gt;</code></p>\n"
      it "CM328" $
        let s = "```foo``\n"
         in s ~-> err 8 (ueib <> etok '`' <> ecsc)
      it "CM329" $
        let s = "`foo\n"
         in s ~-> err 4 (ueib <> etok '`' <> ecsc)
      it "CM330" $
        let s = "`foo``bar``\n"
         in s ~-> err 11 (ueib <> etok '`' <> ecsc)
    context "6.4 Emphasis and strong emphasis" $ do
      it "CM331" $
        "*foo bar*" ==-> "<p><em>foo bar</em></p>\n"
      it "CM332" $
        let s = "a * foo bar*\n"
         in s ~-> errFancy 2 (nonFlanking "*")
      it "CM333" $
        let s = "a*\"foo\"*\n"
         in s ~-> errFancy 1 (nonFlanking "*")
      it "CM334" $
        let s = "* a *\n"
         in s ~-> errFancy 0 (nonFlanking "*")
      it "CM335" $
        let s = "foo*bar*\n"
         in s ~-> errFancy 3 (nonFlanking "*")
      it "CM336" $
        let s = "5*6*78\n"
         in s ~-> errFancy 1 (nonFlanking "*")
      it "CM337" $
        "_foo bar_" ==-> "<p><em>foo bar</em></p>\n"
      it "CM338" $
        let s = "_ foo bar_\n"
         in s ~-> errFancy 0 (nonFlanking "_")
      it "CM339" $
        let s = "a_\"foo\"_\n"
         in s ~-> errFancy 1 (nonFlanking "_")
      it "CM340" $
        let s = "foo_bar_\n"
         in s ~-> errFancy 3 (nonFlanking "_")
      it "CM341" $
        let s = "5_6_78\n"
         in s ~-> errFancy 1 (nonFlanking "_")
      it "CM342" $
        let s = "пристаням_стремятся_\n"
         in s ~-> errFancy 9 (nonFlanking "_")
      it "CM343" $
        let s = "aa_\"bb\"_cc\n"
         in s ~-> errFancy 2 (nonFlanking "_")
      it "CM344" $
        let s = "foo-_(bar)_\n"
         in s ~-> errFancy 4 (nonFlanking "_")
      it "CM345" $
        let s = "_foo*\n"
         in s ~-> err 4 (utok '*' <> etok '_' <> eic)
      it "CM346" $
        let s = "*foo bar *\n"
         in s ~-> errFancy 9 (nonFlanking "*")
      it "CM347" $
        let s = "*foo bar\n*\n"
         in s ~-> err 8 (ueib <> etok '*' <> eic)
      it "CM348" $
        let s = "*(*foo)\n"
         in s ~-> err 7 (ueib <> etok '*' <> eic)
      it "CM349" $
        "*(*foo*)*"
          ==-> "<p><em>(<em>foo</em>)</em></p>\n"
      it "CM350" $
        let s = "*foo*bar\n"
         in s ~-> errFancy 4 (nonFlanking "*")
      it "CM351" $
        let s = "_foo bar _\n"
         in s ~-> errFancy 9 (nonFlanking "_")
      it "CM352" $
        let s = "_(_foo)"
         in s ~-> err 7 (ueib <> etok '_' <> eic)
      it "CM353" $
        "_(_foo_)_"
          ==-> "<p><em>(<em>foo</em>)</em></p>\n"
      it "CM354" $
        let s = "_foo_bar\n"
         in s ~-> errFancy 4 (nonFlanking "_")
      it "CM355" $
        let s = "_пристаням_стремятся\n"
         in s ~-> errFancy 10 (nonFlanking "_")
      it "CM356" $
        let s = "_foo_bar_baz_\n"
         in s ~-> errFancy 4 (nonFlanking "_")
      it "CM357" $
        "_(bar\\)_.\n" ==-> "<p><em>(bar)</em>.</p>\n"
      it "CM358" $
        "**foo bar**\n" ==-> "<p><strong>foo bar</strong></p>\n"
      it "CM359" $
        let s = "** foo bar**\n"
         in s ~-> errFancy 0 (nonFlanking "**")
      it "CM360" $
        let s = "a**\"foo\"**\n"
         in s ~-> errFancy 1 (nonFlanking "**")
      it "CM361" $
        let s = "foo**bar**\n"
         in s ~-> errFancy 3 (nonFlanking "**")
      it "CM362" $
        "__foo bar__" ==-> "<p><strong>foo bar</strong></p>\n"
      it "CM363" $
        let s = "__ foo bar__\n"
         in s ~-> errFancy 0 (nonFlanking "__")
      it "CM364" $
        let s = "__\nfoo bar__\n"
         in s ~-> errFancy 0 (nonFlanking "__")
      it "CM365" $
        let s = "a__\"foo\"__\n"
         in s ~-> errFancy 1 (nonFlanking "__")
      it "CM366" $
        let s = "foo__bar__\n"
         in s ~-> errFancy 3 (nonFlanking "__")
      it "CM367" $
        let s = "5__6__78\n"
         in s ~-> errFancy 1 (nonFlanking "__")
      it "CM368" $
        let s = "пристаням__стремятся__\n"
         in s ~-> errFancy 9 (nonFlanking "__")
      it "CM369" $
        "__foo, __bar__, baz__"
          ==-> "<p><strong>foo, <strong>bar</strong>, baz</strong></p>\n"
      it "CM370" $
        "foo-__\\(bar)__" ==-> "<p>foo-<strong>(bar)</strong></p>\n"
      it "CM371" $
        let s = "**foo bar **\n"
         in s ~-> errFancy 10 (nonFlanking "**")
      it "CM372" $
        let s = "**(**foo)\n"
         in s ~-> err 9 (ueib <> etoks "**" <> eic)
      it "CM373" $
        "*(**foo**)*"
          ==-> "<p><em>(<strong>foo</strong>)</em></p>\n"
      it "CM374" $
        "**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**"
          ==-> "<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.\n<em>Asclepias physocarpa</em>)</strong></p>\n"
      it "CM375" $
        "**foo \"*bar*\" foo**"
          ==-> "<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>\n"
      it "CM376" $
        let s = "**foo**bar\n"
         in s ~-> errFancy 5 (nonFlanking "**")
      it "CM377" $
        let s = "__foo bar __\n"
         in s ~-> errFancy 10 (nonFlanking "__")
      it "CM378" $
        let s = "__(__foo)\n"
         in s ~-> err 9 (ueib <> etoks "__" <> eic)
      it "CM379" $
        "_(__foo__)_"
          ==-> "<p><em>(<strong>foo</strong>)</em></p>\n"
      it "CM380" $
        let s = "__foo__bar\n"
         in s ~-> errFancy 5 (nonFlanking "__")
      it "CM381" $
        let s = "__пристаням__стремятся\n"
         in s ~-> errFancy 11 (nonFlanking "__")
      it "CM382" $
        "__foo\\_\\_bar\\_\\_baz__"
          ==-> "<p><strong>foo__bar__baz</strong></p>\n"
      it "CM383" $
        "__(bar\\)__."
          ==-> "<p><strong>(bar)</strong>.</p>\n"
      it "CM384" $
        "*foo [bar](/url)*"
          ==-> "<p><em>foo <a href=\"/url\">bar</a></em></p>\n"
      it "CM385" $
        "*foo\nbar*"
          ==-> "<p><em>foo\nbar</em></p>\n"
      it "CM386" $
        "_foo __bar__ baz_"
          ==-> "<p><em>foo <strong>bar</strong> baz</em></p>\n"
      it "CM387" $
        "_foo _bar_ baz_"
          ==-> "<p><em>foo <em>bar</em> baz</em></p>\n"
      it "CM388" $
        let s = "__foo_ bar_"
         in s ~-> err 5 (utoks "_ " <> etoks "__" <> eic)
      it "CM389" $
        "*foo *bar**"
          ==-> "<p><em>foo <em>bar</em></em></p>\n"
      it "CM390" $
        "*foo **bar** baz*"
          ==-> "<p><em>foo <strong>bar</strong> baz</em></p>\n"
      it "CM391" $
        let s = "*foo**bar**baz*\n"
         in s ~-> errFancy 5 (nonFlanking "*")
      it "CM392" $
        "***foo** bar*\n" ==-> "<p><em><strong>foo</strong> bar</em></p>\n"
      it "CM393" $
        "*foo **bar***\n" ==-> "<p><em>foo <strong>bar</strong></em></p>\n"
      it "CM394" $
        let s = "*foo**bar***\n"
         in s ~-> errFancy 5 (nonFlanking "*")
      it "CM395" $
        "*foo **bar *baz* bim** bop*\n"
          ==-> "<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>\n"
      it "CM396" $
        "*foo [*bar*](/url)*\n"
          ==-> "<p><em>foo <a href=\"/url\"><em>bar</em></a></em></p>\n"
      it "CM397" $
        let s = "** is not an empty emphasis\n"
         in s ~-> errFancy 0 (nonFlanking "**")
      it "CM398" $
        let s = "**** is not an empty strong emphasis\n"
         in s ~-> errFancy 0 (nonFlanking "****")
      it "CM399" $
        "**foo [bar](/url)**"
          ==-> "<p><strong>foo <a href=\"/url\">bar</a></strong></p>\n"
      it "CM400" $
        "**foo\nbar**"
          ==-> "<p><strong>foo\nbar</strong></p>\n"
      it "CM401" $
        "__foo _bar_ baz__"
          ==-> "<p><strong>foo <em>bar</em> baz</strong></p>\n"
      it "CM402" $
        "__foo __bar__ baz__"
          ==-> "<p><strong>foo <strong>bar</strong> baz</strong></p>\n"
      it "CM403" $
        "____foo__ bar__"
          ==-> "<p><strong><strong>foo</strong> bar</strong></p>\n"
      it "CM404" $
        "**foo **bar****"
          ==-> "<p><strong>foo <strong>bar</strong></strong></p>\n"
      it "CM405" $
        "**foo *bar* baz**"
          ==-> "<p><strong>foo <em>bar</em> baz</strong></p>\n"
      it "CM406" $
        let s = "**foo*bar*baz**\n"
         in s ~-> err 5 (utoks "*b" <> etoks "**" <> eic)
      it "CM407" $
        "***foo* bar**"
          ==-> "<p><strong><em>foo</em> bar</strong></p>\n"
      it "CM408" $
        "**foo *bar***"
          ==-> "<p><strong>foo <em>bar</em></strong></p>\n"
      it "CM409" $
        "**foo *bar **baz**\nbim* bop**"
          ==-> "<p><strong>foo <em>bar <strong>baz</strong>\nbim</em> bop</strong></p>\n"
      it "CM410" $
        "**foo [*bar*](/url)**"
          ==-> "<p><strong>foo <a href=\"/url\"><em>bar</em></a></strong></p>\n"
      it "CM411" $
        let s = "__ is not an empty emphasis\n"
         in s ~-> errFancy 0 (nonFlanking "__")
      it "CM412" $
        let s = "____ is not an empty strong emphasis\n"
         in s ~-> errFancy 0 (nonFlanking "____")
      it "CM413" $
        let s = "foo ***\n"
         in s ~-> errFancy 4 (nonFlanking "***")
      it "CM414" $
        "foo *\\**" ==-> "<p>foo <em>*</em></p>\n"
      it "CM415" $
        "foo *\\_*\n" ==-> "<p>foo <em>_</em></p>\n"
      it "CM416" $
        let s = "foo *****\n"
         in s ~-> errFancy 8 (nonFlanking "*")
      it "CM417" $
        "foo **\\***" ==-> "<p>foo <strong>*</strong></p>\n"
      it "CM418" $
        "foo **\\_**\n" ==-> "<p>foo <strong>_</strong></p>\n"
      it "CM419" $
        let s = "**foo*\n"
         in s ~-> err 5 (utok '*' <> etoks "**" <> eic)
      it "CM420" $
        let s = "*foo**\n"
         in s ~-> errFancy 5 (nonFlanking "*")
      it "CM421" $
        let s = "***foo**\n"
         in s ~-> err 8 (ueib <> etok '*' <> eic)
      it "CM422" $
        let s = "****foo*\n"
         in s ~-> err 7 (utok '*' <> etoks "**" <> eic)
      it "CM423" $
        let s = "**foo***\n"
         in s ~-> errFancy 7 (nonFlanking "*")
      it "CM424" $
        let s = "*foo****\n"
         in s ~-> errFancy 5 (nonFlanking "***")
      it "CM425" $
        let s = "foo ___\n"
         in s ~-> errFancy 4 (nonFlanking "___")
      it "CM426" $
        "foo _\\__" ==-> "<p>foo <em>_</em></p>\n"
      it "CM427" $
        "foo _\\*_" ==-> "<p>foo <em>*</em></p>\n"
      it "CM428" $
        let s = "foo _____\n"
         in s ~-> errFancy 8 (nonFlanking "_")
      it "CM429" $
        "foo __\\___" ==-> "<p>foo <strong>_</strong></p>\n"
      it "CM430" $
        "foo __\\*__" ==-> "<p>foo <strong>*</strong></p>\n"
      it "CM431" $
        let s = "__foo_\n"
         in s ~-> err 5 (utok '_' <> etoks "__" <> eic)
      it "CM432" $
        let s = "_foo__\n"
         in s ~-> errFancy 5 (nonFlanking "_")
      it "CM433" $
        let s = "___foo__\n"
         in s ~-> err 8 (ueib <> etok '_' <> eic)
      it "CM434" $
        let s = "____foo_\n"
         in s ~-> err 7 (utok '_' <> etoks "__" <> eic)
      it "CM435" $
        let s = "__foo___\n"
         in s ~-> errFancy 7 (nonFlanking "_")
      it "CM436" $
        let s = "_foo____\n"
         in s ~-> errFancy 5 (nonFlanking "___")
      it "CM437" $
        "**foo**" ==-> "<p><strong>foo</strong></p>\n"
      it "CM438" $
        "*_foo_*" ==-> "<p><em><em>foo</em></em></p>\n"
      it "CM439" $
        "__foo__" ==-> "<p><strong>foo</strong></p>\n"
      it "CM440" $
        "_*foo*_" ==-> "<p><em><em>foo</em></em></p>\n"
      it "CM441" $
        "****foo****" ==-> "<p><strong><strong>foo</strong></strong></p>\n"
      it "CM442" $
        "____foo____" ==-> "<p><strong><strong>foo</strong></strong></p>\n"
      it "CM443" $
        "******foo******"
          ==-> "<p><strong><strong><strong>foo</strong></strong></strong></p>\n"
      it "CM444" $
        "***foo***" ==-> "<p><em><strong>foo</strong></em></p>\n"
      it "CM445" $
        "_____foo_____"
          ==-> "<p><strong><strong><em>foo</em></strong></strong></p>\n"
      it "CM446" $
        let s = "*foo _bar* baz_\n"
         in s ~-> err 9 (utok '*' <> etok '_' <> eic)
      it "CM447" $
        let s = "*foo __bar *baz bim__ bam*\n"
         in s ~-> err 19 (utok '_' <> etok '*' <> eic)
      it "CM448" $
        let s = "**foo **bar baz**\n"
         in s ~-> err 17 (ueib <> etoks "**" <> eic)
      it "CM449" $
        let s = "*foo *bar baz*\n"
         in s ~-> err 14 (ueib <> etok '*' <> eic)
      it "CM450" $
        let s = "*[bar*](/url)\n"
         in s ~-> err 5 (utok '*' <> etok ']' <> eic)
      it "CM451" $
        let s = "_foo [bar_](/url)\n"
         in s ~-> err 9 (utok '_' <> etok ']' <> eic)
      it "CM452" $
        let s = "*<img src=\"foo\" title=\"*\"/>\n"
         in s ~-> errFancy 23 (nonFlanking "*")
      it "CM453" $
        let s = "**<a href=\"**\">"
         in s ~-> errFancy 11 (nonFlanking "**")
      it "CM454" $
        let s = "__<a href=\"__\">\n"
         in s ~-> errFancy 11 (nonFlanking "__")
      it "CM455" $
        "*a `*`*" ==-> "<p><em>a <code>*</code></em></p>\n"
      it "CM456" $
        "_a `_`_" ==-> "<p><em>a <code>_</code></em></p>\n"
      it "CM457" $
        let s = "**a<http://foo.bar/?q=**>"
         in s ~-> err 25 (ueib <> etoks "**" <> eic)
      it "CM458" $
        let s = "__a<http://foo.bar/?q=__>"
         in s ~-> err 25 (ueib <> etoks "__" <> eic)
    context "6.5 Links" $ do
      it "CM459" $
        "[link](/uri \"title\")"
          ##-> p_ (a_ [href_ "/uri", title_ "title"] "link")
      it "CM460" $
        "[link](/uri)"
          ==-> "<p><a href=\"/uri\">link</a></p>\n"
      it "CM461" $
        let s = "[link]()"
         in s
              ~-> err
                7
                (utok ')' <> etok '<' <> elabel "URI" <> ews)
      it "CM462" $
        "[link](<>)"
          ==-> "<p><a href>link</a></p>\n"
      it "CM463" $
        let s = "[link](/my uri)\n"
         in s
              ~-> err
                11
                (utok 'u' <> etok '"' <> etok '\'' <> etok '(' <> etok ')' <> ews)
      it "CM464" $
        let s = "[link](</my uri>)\n"
         in s ~-> err 11 (utok ' ' <> euric <> etok '>')
      it "CM465" $
        let s = "[link](foo\nbar)\n"
         in s
              ~-> err
                11
                (utok 'b' <> etok '"' <> etok '\'' <> etok '(' <> etok ')' <> ews)
      it "CM466" $
        let s = "[link](<foo\nbar>)\n"
         in s ~-> err 11 (utok '\n' <> euric <> etok '>')
      it "CM467" $
        let s = "[link](\\(foo\\))"
         in s
              ~-> err
                7
                ( utok '\\'
                    <> etoks "//"
                    <> etok '#'
                    <> etok '/'
                    <> etok '<'
                    <> etok '?'
                    <> elabel "ASCII alpha character"
                    <> euri
                    <> elabel "path piece"
                    <> ews
                )
      it "CM468" $
        "[link](foo(and(bar)))\n"
          ==-> "<p><a href=\"foo%28and%28bar\">link</a>))</p>\n"
      it "CM469" $
        let s = "[link](foo\\(and\\(bar\\))"
         in s ~-> err 10 (utok '\\' <> euric <> euri)
      it "CM470" $
        "[link](<foo(and(bar)>)"
          ==-> "<p><a href=\"foo%28and%28bar%29\">link</a></p>\n"
      it "CM471" $
        let s = "[link](foo\\)\\:)"
         in s ~-> err 10 (utok '\\' <> euric <> euri)
      it "CM472" $
        "[link](#fragment)\n\n[link](http://example.com#fragment)\n\n[link](http://example.com?foo=3#frag)\n"
          ==-> "<p><a href=\"#fragment\">link</a></p>\n<p><a href=\"http://example.com#fragment\">link</a></p>\n<p><a href=\"http://example.com?foo=3#frag\">link</a></p>\n"
      it "CM473" $
        let s = "[link](foo\\bar)"
         in s ~-> err 10 (utok '\\' <> euric <> euri)
      it "CM474" $
        "[link](foo%20b&auml;)"
          ==-> "<p><a href=\"foo%20b%26auml%3b\">link</a></p>\n"
      it "CM475" $
        let s = "[link](\"title\")"
         in s
              ~-> err
                7
                ( utok '"'
                    <> etoks "//"
                    <> etok '#'
                    <> etok '/'
                    <> etok '<'
                    <> etok '?'
                    <> elabel "ASCII alpha character"
                    <> euri
                    <> elabel "path piece"
                    <> ews
                )
      it "CM476" $
        "[link](/url \"title\")\n[link](/url 'title')\n[link](/url (title))"
          ##-> p_
            ( do
                a_ [href_ "/url", title_ "title"] "link"
                "\n"
                a_ [href_ "/url", title_ "title"] "link"
                "\n"
                a_ [href_ "/url", title_ "title"] "link"
            )
      it "CM477" $
        "[link](/url \"title \\\"&quot;\")\n"
          ##-> p_ (a_ [href_ "/url", title_ "title \"\""] "link")
      it "CM478" $
        let s = "[link](/url \"title\")"
         in s ~-> err 11 (utok ' ' <> euric <> euri)
      it "CM479" $
        let s = "[link](/url \"title \"and\" title\")\n"
         in s ~-> err 20 (utok 'a' <> etok ')' <> ews)
      it "CM480" $
        "[link](/url 'title \"and\" title')"
          ##-> p_ (a_ [href_ "/url", title_ "title \"and\" title"] "link")
      it "CM481" $
        "[link](   /uri\n  \"title\"  )"
          ##-> p_ (a_ [href_ "/uri", title_ "title"] "link")
      it "CM482" $
        let s = "[link] (/uri)\n"
         in s ~-> errFancy 1 (couldNotMatchRef "link" [])
      it "CM483" $
        let s = "[link [foo [bar]]](/uri)\n"
         in s ~-> err 6 (utok '[' <> etok ']' <> eic)
      it "CM484" $
        let s = "[link] bar](/uri)\n"
         in s ~-> errFancy 1 (couldNotMatchRef "link" [])
      it "CM485" $
        let s = "[link [bar](/uri)\n"
         in s ~-> err 6 (utok '[' <> etok ']' <> eic)
      it "CM486" $
        "[link \\[bar](/uri)\n"
          ==-> "<p><a href=\"/uri\">link [bar</a></p>\n"
      it "CM487" $
        "[link *foo **bar** `#`*](/uri)"
          ==-> "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"
      it "CM488" $
        "[![moon](moon.jpg)](/uri)"
          ==-> "<p><a href=\"/uri\"><img alt=\"moon\" src=\"moon.jpg\"></a></p>\n"
      it "CM489" $
        let s = "[foo [bar](/uri)](/uri)\n"
         in s ~-> err 5 (utok '[' <> etok ']' <> eic)
      it "CM490" $
        let s = "[foo *[bar [baz](/uri)](/uri)*](/uri)\n"
         in s ~-> err 6 (utok '[' <> eic)
      it "CM491" $
        let s = "![[[foo](uri1)](uri2)](uri3)"
         in s ~-> err 3 (utok '[' <> eic)
      it "CM492" $
        let s = "*[foo*](/uri)\n"
         in s ~-> err 5 (utok '*' <> etok ']' <> eic)
      it "CM493" $
        let s = "[foo *bar](baz*)\n"
         in s ~-> err 9 (utok ']' <> etok '*' <> eic)
      it "CM494" $
        let s = "*foo [bar* baz]\n"
         in s ~-> err 9 (utok '*' <> etok ']' <> eic)
      it "CM495" $
        "[foo <bar attr=\"](baz)\">"
          ==-> "<p><a href=\"baz\">foo &lt;bar attr=&quot;</a>&quot;&gt;</p>\n"
      it "CM496" $
        let s = "[foo`](/uri)`\n"
         in s ~-> err 13 (ueib <> etok ']' <> eic)
      it "CM497" $
        "[foo<http://example.com/?search=](uri)>"
          ==-> "<p><a href=\"uri\">foo&lt;http://example.com/?search=</a>&gt;</p>\n"
      it "CM498" $
        "[foo][bar]\n\n[bar]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "foo")
      it "CM499" $
        let s = "[link [foo [bar]]][ref]\n\n[ref]: /uri"
         in s ~-> err 6 (utok '[' <> etok ']' <> eic)
      it "CM500" $
        "[link \\[bar][ref]\n\n[ref]: /uri"
          ==-> "<p><a href=\"/uri\">link [bar</a></p>\n"
      it "CM501" $
        "[link *foo **bar** `#`*][ref]\n\n[ref]: /uri"
          ==-> "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"
      it "CM502" $
        "[![moon](moon.jpg)][ref]\n\n[ref]: /uri"
          ==-> "<p><a href=\"/uri\"><img alt=\"moon\" src=\"moon.jpg\"></a></p>\n"
      it "CM503" $
        let s = "[foo [bar](/uri)][ref]\n\n[ref]: /uri"
         in s ~-> err 5 (utok '[' <> etok ']' <> eic)
      it "CM504" $
        let s = "[foo *bar [baz][ref]*][ref]\n\n[ref]: /uri"
         in s ~-> err 10 (utok '[' <> etok '*' <> eic)
      it "CM505" $
        let s = "*[foo*][ref]\n\n[ref]: /uri"
         in s ~-> err 5 (utok '*' <> etok ']' <> eic)
      it "CM506" $
        let s = "[foo *bar][ref]\n\n[ref]: /uri"
         in s ~-> err 9 (utok ']' <> etok '*' <> eic)
      it "CM507" $
        "[foo <bar attr=\"][ref]\">\n\n[ref]: /uri"
          ==-> "<p><a href=\"/uri\">foo &lt;bar attr=&quot;</a>&quot;&gt;</p>\n"
      it "CM508" $
        let s = "[foo`][ref]`\n\n[ref]: /uri"
         in s ~-> err 12 (ueib <> etok ']' <> eic)
      it "CM509" $
        "[foo<http://example.com/?search=][ref]>\n\n[ref]: /uri"
          ==-> "<p><a href=\"/uri\">foo&lt;http://example.com/?search=</a>&gt;</p>\n"
      it "CM510" $
        "[foo][BaR]\n\n[bar]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "foo")
      it "CM511" $
        "[Толпой][Толпой] is a Russian word.\n\n[ТОЛПОЙ]: /url"
          ==-> "<p><a href=\"/url\">Толпой</a> is a Russian word.</p>\n"
      it "CM512" $
        "[Foo\n  bar]: /url\n\n[Baz][Foo bar]"
          ==-> "<p><a href=\"/url\">Baz</a></p>\n"
      it "CM513" $
        let s = "[foo] [bar]\n\n[bar]: /url \"title\""
         in s ~-> errFancy 1 (couldNotMatchRef "foo" [])
      it "CM514" $
        let s = "[foo]\n[bar]\n\n[bar]: /url \"title\""
         in s ~-> errFancy 1 (couldNotMatchRef "foo" [])
      it "CM515" $
        let s = "[foo]: /url1\n\n[foo]: /url2\n\n[bar][foo]"
         in s ~-> errFancy 15 (duplicateRef "foo")
      it "CM516" $
        "[bar][foo\\!]\n\n[foo!]: /url"
          ==-> "<p><a href=\"/url\">bar</a></p>\n"
      it "CM517" $
        let s = "[foo][ref[]\n\n[ref[]: /uri"
         in s
              ~~-> [ err
                       9
                       ( utok '['
                           <> etoks "&#"
                           <> etok '&'
                           <> etok ']'
                           <> elabel "escaped character"
                       ),
                     err 17 (utok '[' <> etok ']' <> eic)
                   ]
      it "CM518" $
        let s = "[foo][ref[bar]]\n\n[ref[bar]]: /uri"
         in s
              ~~-> [ err
                       9
                       ( utok '['
                           <> etoks "&#"
                           <> etok '&'
                           <> etok ']'
                           <> elabel "escaped character"
                       ),
                     err 21 (utok '[' <> etok ']' <> eic)
                   ]
      it "CM519" $
        let s = "[[[foo]]]\n\n[[[foo]]]: /url"
         in s
              ~~-> [ err 1 (utok '[' <> eic),
                     err 12 (utok '[' <> eic)
                   ]
      it "CM520" $
        "[foo][ref\\[]\n\n[ref\\[]: /uri"
          ==-> "<p><a href=\"/uri\">foo</a></p>\n"
      it "CM521" $
        "[bar\\\\]: /uri\n\n[bar\\\\]"
          ==-> "<p><a href=\"/uri\">bar\\</a></p>\n"
      it "CM522" $
        let s = "[]\n\n[]: /uri"
         in s
              ~~-> [ err 1 (utok ']' <> eic),
                     err 5 (utok ']' <> eic)
                   ]
      it "CM523" $
        let s = "[\n ]\n\n[\n ]: /uri"
         in s
              ~~-> [ errFancy 1 (couldNotMatchRef "" []),
                     errFancy 7 (couldNotMatchRef "" [])
                   ]
      it "CM524" $
        "[foo][]\n\n[foo]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "foo")
      it "CM525" $
        let s = "[*foo* bar][]\n\n[*foo* bar]: /url \"title\""
         in s ~-> errFancy 1 (couldNotMatchRef "foo bar" ["*foo* bar"])
      it "CM526" $
        "[Foo][]\n\n[foo]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "Foo")
      it "CM527" $
        let s = "[foo] \n[]\n\n[foo]: /url \"title\""
         in s ~-> err 8 (utok ']' <> eic)
      it "CM528" $
        "[foo]\n\n[foo]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "foo")
      it "CM529" $
        let s = "[*foo* bar]\n\n[*foo* bar]: /url \"title\""
         in s ~-> errFancy 1 (couldNotMatchRef "foo bar" ["*foo* bar"])
      it "CM530" $
        let s = "[[*foo* bar]]\n\n[*foo* bar]: /url \"title\""
         in s ~-> err 1 (utok '[' <> eic)
      it "CM531" $
        let s = "[[bar [foo]\n\n[foo]: /url"
         in s ~-> err 1 (utok '[' <> eic)
      it "CM532" $
        "[Foo]\n\n[foo]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "Foo")
      it "CM533" $
        "[foo] bar\n\n[foo]: /url"
          ==-> "<p><a href=\"/url\">foo</a> bar</p>\n"
      it "CM534" $
        let s = "\\[foo]\n\n[foo]: /url \"title\""
         in s ~-> err 5 (utok ']' <> eeib <> eic)
      it "CM535" $
        let s = "[foo*]: /url\n\n*[foo*]"
         in s ~-> err 19 (utok '*' <> etok ']' <> eic)
      it "CM536" $
        "[foo][bar]\n\n[foo]: /url1\n[bar]: /url2"
          ==-> "<p><a href=\"/url2\">foo</a></p>\n"
      it "CM537" $
        "[foo][]\n\n[foo]: /url1"
          ==-> "<p><a href=\"/url1\">foo</a></p>\n"
      it "CM538" $
        let s = "[foo]()\n\n[foo]: /url1"
         in s ~-> err 6 (utok ')' <> etok '<' <> elabel "URI" <> ews)
      it "CM539" $
        let s = "[foo](not a link)\n\n[foo]: /url1"
         in s
              ~-> err
                10
                (utok 'a' <> etok '"' <> etok '\'' <> etok '(' <> etok ')' <> ews)
      it "CM540" $
        let s = "[foo][bar][baz]\n\n[baz]: /url"
         in s ~-> errFancy 6 (couldNotMatchRef "bar" ["baz"])
      it "CM541" $
        "[foo][bar][baz]\n\n[baz]: /url1\n[bar]: /url2"
          ==-> "<p><a href=\"/url2\">foo</a><a href=\"/url1\">baz</a></p>\n"
      it "CM542" $
        let s = "[foo][bar][baz]\n\n[baz]: /url1\n[foo]: /url2"
         in s ~-> errFancy 6 (couldNotMatchRef "bar" ["baz"])
    context "6.6 Images" $ do
      it "CM543" $
        "![foo](/url \"title\")"
          ==-> "<p><img alt=\"foo\" src=\"/url\" title=\"title\"></p>\n"
      it "CM544" $
        "![foo *bar*](train.jpg \"train & tracks\")"
          ==-> "<p><img alt=\"foo bar\" src=\"train.jpg\" title=\"train &amp; tracks\"></p>\n"
      it "CM545" $
        let s = "![foo ![bar](/url)](/url2)\n"
         in s ~-> err 6 (utok '!' <> etok ']' <> eic)
      it "CM546" $
        "![foo [bar](/url)](/url2)"
          ==-> "<p><img alt=\"foo bar\" src=\"/url2\"></p>\n"
      it "CM547" $
        let s = "![foo *bar*][]\n\n[foo *bar*]: train.jpg \"train & tracks\"\n"
         in s ~-> errFancy 2 (couldNotMatchRef "foo bar" ["foo *bar*"])
      it "CM548" $
        "![foo *bar*][foobar]\n\n[FOOBAR]: train.jpg \"train & tracks\""
          ==-> "<p><img alt=\"foo bar\" src=\"train.jpg\" title=\"train &amp; tracks\"></p>\n"
      it "CM549" $
        "![foo](train.jpg)"
          ==-> "<p><img alt=\"foo\" src=\"train.jpg\"></p>\n"
      it "CM550" $
        "My ![foo bar](/path/to/train.jpg  \"title\"   )"
          ==-> "<p>My <img alt=\"foo bar\" src=\"/path/to/train.jpg\" title=\"title\"></p>\n"
      it "CM551" $
        "![foo](<url>)"
          ==-> "<p><img alt=\"foo\" src=\"url\"></p>\n"
      it "CM552" $
        "![](/url)" ==-> "<p><img alt src=\"/url\"></p>\n"
      it "CM553" $
        "![foo][bar]\n\n[bar]: /url"
          ==-> "<p><img alt=\"foo\" src=\"/url\"></p>\n"
      it "CM554" $
        "![foo][bar]\n\n[BAR]: /url"
          ==-> "<p><img alt=\"foo\" src=\"/url\"></p>\n"
      it "CM555" $
        "![foo][]\n\n[foo]: /url \"title\""
          ==-> "<p><img alt=\"foo\" src=\"/url\" title=\"title\"></p>\n"
      it "CM556" $
        "![foo bar][]\n\n[foo bar]: /url \"title\""
          ==-> "<p><img alt=\"foo bar\" src=\"/url\" title=\"title\"></p>\n"
      it "CM557" $
        "![Foo][]\n\n[foo]: /url \"title\""
          ==-> "<p><img alt=\"Foo\" src=\"/url\" title=\"title\"></p>\n"
      it "CM558" $
        let s = "![foo] \n[]\n\n[foo]: /url \"title\""
         in s ~-> err 9 (utok ']' <> eic)
      it "CM559" $
        "![foo]\n\n[foo]: /url \"title\""
          ==-> "<p><img alt=\"foo\" src=\"/url\" title=\"title\"></p>\n"
      it "CM560" $
        "![*foo* bar]\n\n[foo bar]: /url \"title\"\n"
          ==-> "<p><img alt=\"foo bar\" src=\"/url\" title=\"title\"></p>\n"
      it "CM561" $
        let s = "![[foo]]\n\n[[foo]]: /url \"title\""
         in s
              ~~-> [ errFancy 3 (couldNotMatchRef "foo" []),
                     err 11 (utok '[' <> eic)
                   ]
      it "CM562" $
        "![Foo]\n\n[foo]: /url \"title\""
          ==-> "<p><img alt=\"Foo\" src=\"/url\" title=\"title\"></p>\n"
      it "CM563" $
        "!\\[foo\\]\n\n[foo]: /url \"title\""
          ==-> "<p>![foo]</p>\n"
      it "CM564" $
        "\\![foo]\n\n[foo]: /url \"title\""
          ##-> p_
            ( do
                "!"
                a_ [href_ "/url", title_ "title"] "foo"
            )
    context "6.7 Autolinks" $ do
      it "CM565" $
        "<http://foo.bar.baz>"
          ==-> "<p><a href=\"http://foo.bar.baz\">http://foo.bar.baz</a></p>\n"
      it "CM566" $
        "<http://foo.bar.baz/test?q=hello&id=22&boolean>"
          ==-> "<p><a href=\"http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean\">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>\n"
      it "CM567" $
        "<irc://foo.bar:2233/baz>"
          ==-> "<p><a href=\"irc://foo.bar:2233/baz\">irc://foo.bar:2233/baz</a></p>\n"
      it "CM568" $
        "<MAILTO:FOO@BAR.BAZ>"
          ==-> "<p><a href=\"mailto:FOO@BAR.BAZ\">FOO@BAR.BAZ</a></p>\n"
      it "CM569" $
        "<a+b+c:d>"
          ==-> "<p><a href=\"a+b+c:d\">a+b+c:d</a></p>\n"
      it "CM570" $
        "<made-up-scheme://foo,bar>"
          ==-> "<p><a href=\"made-up-scheme://foo/%2cbar\">made-up-scheme://foo/%2cbar</a></p>\n"
      it "CM571" $
        "<http://../>"
          ==-> "<p><a href=\"http://..\">http://..</a></p>\n"
      it "CM572" $
        "<localhost:5001/foo>"
          ==-> "<p><a href=\"localhost:5001/foo\">localhost:5001/foo</a></p>\n"
      it "CM573" $
        "<http://foo.bar/baz bim>\n"
          ==-> "<p>&lt;http://foo.bar/baz bim&gt;</p>\n"
      it "CM574" $
        "<http://example.com/\\[\\>"
          ==-> "<p>&lt;http://example.com/[&gt;</p>\n"
      it "CM575" $
        "<foo@bar.example.com>"
          ==-> "<p><a href=\"mailto:foo@bar.example.com\">foo@bar.example.com</a></p>\n"
      it "CM576" $
        "<foo+special@Bar.baz-bar0.com>"
          ==-> "<p><a href=\"mailto:foo%2bspecial@Bar.baz-bar0.com\">foo+special@Bar.baz-bar0.com</a></p>\n"
      it "CM577" $
        "<foo\\+@bar.example.com>"
          ==-> "<p>&lt;foo+@bar.example.com&gt;</p>\n"
      it "CM578" $
        "<>"
          ==-> "<p>&lt;&gt;</p>\n"
      it "CM579" $
        "< http://foo.bar >"
          ==-> "<p>&lt; http://foo.bar &gt;</p>\n"
      it "CM580" $
        "<m:abc>"
          ==-> "<p><a href=\"m:abc\">m:abc</a></p>\n"
      it "CM581" $
        "<foo.bar.baz>"
          ==-> "<p><a href=\"foo.bar.baz\">foo.bar.baz</a></p>\n"
      it "CM582" $
        "http://example.com"
          ==-> "<p>http://example.com</p>\n"
      it "CM583" $
        "foo@bar.example.com"
          ==-> "<p>foo@bar.example.com</p>\n"
    context "6.8 Raw HTML" $
      -- NOTE We do not support raw HTML, see the readme.
      return ()
    context "6.9 Hard line breaks" $ do
      -- NOTE We currently do not support hard line breaks represented in
      -- markup as two spaces before newline.
      it "CM605" $
        "foo  \nbaz"
          ==-> "<p>foo\nbaz</p>\n"
      it "CM606" $
        "foo\\\nbaz\n"
          ==-> "<p>foo<br>\nbaz</p>\n"
      it "CM607" $
        "foo       \nbaz"
          ==-> "<p>foo\nbaz</p>\n"
      it "CM608" $
        "foo  \n     bar"
          ==-> "<p>foo\nbar</p>\n"
      it "CM609" $
        "foo\\\n     bar"
          ==-> "<p>foo<br>\nbar</p>\n"
      it "CM610" $
        "*foo  \nbar*"
          ==-> "<p><em>foo\nbar</em></p>\n"
      it "CM611" $
        "*foo\\\nbar*"
          ==-> "<p><em>foo<br>\nbar</em></p>\n"
      it "CM612" $
        "`code  \nspan`"
          ==-> "<p><code>code span</code></p>\n"
      it "CM613" $
        "`code\\\nspan`"
          ==-> "<p><code>code\\ span</code></p>\n"
      it "CM614" $
        "<a href=\"foo  \nbar\">"
          ==-> "<p>&lt;a href=&quot;foo\nbar&quot;&gt;</p>\n"
      it "CM615" $
        "<a href=\"foo\\\nbar\">"
          ==-> "<p>&lt;a href=&quot;foo<br>\nbar&quot;&gt;</p>\n"
      it "CM616" $
        "foo\\"
          ==-> "<p>foo\\</p>\n"
      it "CM617" $
        "foo  "
          ==-> "<p>foo</p>\n"
      it "CM618" $
        "### foo\\"
          ==-> "<h3 id=\"foo\">foo\\</h3>\n"
      it "CM619" $
        "### foo  "
          ==-> "<h3 id=\"foo\">foo</h3>\n"
    context "6.10 Soft line breaks" $ do
      it "CM620" $
        "foo\nbaz"
          ==-> "<p>foo\nbaz</p>\n"
      it "CM621" $
        "foo \n baz"
          ==-> "<p>foo\nbaz</p>\n"
    context "6.11 Textual content" $ do
      it "CM622" $
        "hello $.;'there"
          ==-> "<p>hello $.;&#39;there</p>\n"
      it "CM623" $
        "Foo χρῆν"
          ==-> "<p>Foo χρῆν</p>\n"
      it "CM624" $
        "Multiple     spaces"
          ==-> "<p>Multiple     spaces</p>\n"
    -- NOTE I don't test these so extensively because they share
    -- implementation with emphasis and strong emphasis which are thoroughly
    -- tested already.
    context "strikeout" $ do
      it "works in simplest form" $
        "It's ~~bad~~ news."
          ==-> "<p>It&#39;s <del>bad</del> news.</p>\n"
      it "combines with emphasis" $
        "**It's ~~bad~~** news."
          ==-> "<p><strong>It&#39;s <del>bad</del></strong> news.</p>\n"
      it "interacts with subscript reasonably (1)" $
        "It's ~~~bad~~ news~."
          ==-> "<p>It&#39;s <sub><del>bad</del> news</sub>.</p>\n"
      it "interacts with subscript reasonably (2)" $
        "It's ~~~bad~ news~~."
          ==-> "<p>It&#39;s <del><sub>bad</sub> news</del>.</p>\n"
    context "subscript" $ do
      it "works in simplest form" $
        "It's ~bad~ news."
          ==-> "<p>It&#39;s <sub>bad</sub> news.</p>\n"
      it "combines with emphasis" $
        "**It's ~bad~** news."
          ==-> "<p><strong>It&#39;s <sub>bad</sub></strong> news.</p>\n"
    context "superscript" $ do
      it "works in simplest form" $
        "It's ^bad^ news."
          ==-> "<p>It&#39;s <sup>bad</sup> news.</p>\n"
      it "combines with emphasis" $
        "**It's ^bad^** news."
          ==-> "<p><strong>It&#39;s <sup>bad</sup></strong> news.</p>\n"
      it "a composite, complex example" $
        "***Something ~~~is not~~ going~ ^so well^** today*."
          ==-> "<p><em><strong>Something <sub><del>is not</del> going</sub> <sup>so well</sup></strong> today</em>.</p>\n"
    context "collapsed reference links (special cases)" $
      it "offsets after such links are still correct" $
        "[foo][] *foo\n\n[foo]: https://example.org"
          ~-> err
            12
            (ueib <> etok '*' <> eic)
    context "title parse errors" $
      it "parse error is OK in reference definitions" $
        let s = "[something]: something something"
         in s
              ~-> err
                23
                ( utoks "so"
                    <> etok '\''
                    <> etok '\"'
                    <> etok '('
                    <> elabel "white space"
                    <> elabel "newline"
                )
    context "tables" $ do
      it "recognizes single column tables" $ do
        let o = "<table>\n<thead>\n<tr><th>Foo</th></tr>\n</thead>\n<tbody>\n<tr><td>foo</td></tr>\n</tbody>\n</table>\n"
        "|Foo\n---\nfoo" ==-> o
        "Foo|\n---\nfoo" ==-> o
        "| Foo |\n ---  \n  foo  " ==-> o
        "| Foo |\n| --- |\n| foo |" ==-> o
      it "reports correct parse errors when parsing the header line" $
        ( let s = "Foo | Bar\na-- | ---"
           in s ~-> err 10 (utok 'a' <> etok '-' <> etok ':' <> etok '|' <> elabel "white space")
        )
          >> ( let s = "Foo | Bar\n-a- | ---"
                in s ~-> err 11 (utok 'a' <> etok '-')
             )
          >> ( let s = "Foo | Bar\n--a | ---"
                in s ~-> err 12 (utok 'a' <> etok '-')
             )
          >> ( let s = "Foo | Bar\n---a | ---"
                in s ~-> err 13 (utok 'a' <> etok '-' <> etok ':' <> etok '|' <> elabel "white space")
             )
      it "falls back to paragraph when header line is weird enough" $
        "Foo | Bar\nab- | ---"
          ==-> "<p>Foo | Bar\nab- | ---</p>\n"
      it "demands that number of columns in rows match number of columns in header" $
        ( let s = "Foo | Bar | Baz\n--- | --- | ---\nfoo | bar"
           in s ~-> err 41 (ulabel "end of table block" <> etok '|' <> eic)
        )
          >> ( let s = "Foo | Bar | Baz\n--- | --- | ---\nfoo | bar\n\nHere it goes."
                in s ~-> err 41 (utok '\n' <> etok '|' <> eic)
             )
      it "recognizes escaped pipes" $
        "Foo \\| | Bar\n--- | ---\nfoo | \\|"
          ==-> "<table>\n<thead>\n<tr><th>Foo |</th><th>Bar</th></tr>\n</thead>\n<tbody>\n<tr><td>foo</td><td>|</td></tr>\n</tbody>\n</table>\n"
      it "escaped characters preserve backslashes for inline-level parser" $
        "Foo | Bar\n--- | ---\n\\*foo\\* | bar"
          ==-> "<table>\n<thead>\n<tr><th>Foo</th><th>Bar</th></tr>\n</thead>\n<tbody>\n<tr><td>*foo*</td><td>bar</td></tr>\n</tbody>\n</table>\n"
      it "escaped pipes do not fool position tracking" $
        let s = "Foo | Bar\n--- | ---\n\\| *fo | bar"
         in s ~-> err 26 (ueib <> etok '*' <> elabel "inline content")
      it "pipes in code spans in headers do not fool the parser" $
        "`|Foo|` | `|Bar|`\n--- | ---\nfoo | bar"
          ==-> "<table>\n<thead>\n<tr><th><code>|Foo|</code></th><th><code>|Bar|</code></th></tr>\n</thead>\n<tbody>\n<tr><td>foo</td><td>bar</td></tr>\n</tbody>\n</table>\n"
      it "pipes in code spans in cells do not fool the parser" $
        "Foo | Bar\n--- | ---\n`|foo|` | `|bar|`"
          ==-> "<table>\n<thead>\n<tr><th>Foo</th><th>Bar</th></tr>\n</thead>\n<tbody>\n<tr><td><code>|foo|</code></td><td><code>|bar|</code></td></tr>\n</tbody>\n</table>\n"
      it "multi-line code spans are disallowed in table headers" $
        "`Foo\nBar` | Bar\n--- | ---\nfoo | bar"
          ==-> "<p><code>Foo Bar</code> | Bar\n--- | ---\nfoo | bar</p>\n"
      it "multi-line code spans are disallowed in table cells" $
        let s = "Foo | Bar\n--- | ---\n`foo\nbar` | bar"
         in s
              ~~-> [ err 24 (utok '\n' <> etok '`' <> ecsc),
                     err 35 (ueib <> etok '`' <> ecsc)
                   ]
      it "parses tables with just header row" $
        "Foo | Bar\n--- | ---"
          ==-> "<table>\n<thead>\n<tr><th>Foo</th><th>Bar</th></tr>\n</thead>\n<tbody>\n</tbody>\n</table>\n"
      it "recognizes end of table correctly" $
        "Foo | Bar\n--- | ---\nfoo | bar\n\nHere goes a paragraph."
          ==-> "<table>\n<thead>\n<tr><th>Foo</th><th>Bar</th></tr>\n</thead>\n<tbody>\n<tr><td>foo</td><td>bar</td></tr>\n</tbody>\n</table>\n<p>Here goes a paragraph.</p>\n"
      it "is capable of reporting a parse error per cell" $
        let s = "Foo | *Bar\n--- | ----\n_foo | bar_"
         in s
              ~~-> [ err 10 (ueib <> etok '*' <> eic),
                     err 26 (ueib <> etok '_' <> eic),
                     errFancy 32 (nonFlanking "_")
                   ]
      it "tables have higher precedence than unordered lists" $ do
        "+ foo | bar\n------|----\n"
          ==-> "<table>\n<thead>\n<tr><th>+ foo</th><th>bar</th></tr>\n</thead>\n<tbody>\n</tbody>\n</table>\n"
        "+ foo | bar\n -----|----\n"
          ==-> "<table>\n<thead>\n<tr><th>+ foo</th><th>bar</th></tr>\n</thead>\n<tbody>\n</tbody>\n</table>\n"
      it "tables have higher precedence than ordered lists" $ do
        "1. foo | bar\n-------|----\n"
          ==-> "<table>\n<thead>\n<tr><th>1. foo</th><th>bar</th></tr>\n</thead>\n<tbody>\n</tbody>\n</table>\n"
        "1. foo | bar\n ------|----\n"
          ==-> "<table>\n<thead>\n<tr><th>1. foo</th><th>bar</th></tr>\n</thead>\n<tbody>\n</tbody>\n</table>\n"
      it "if table is indented inside unordered list, it's put there" $
        "+ foo | bar\n  ----|----\n"
          ==-> "<ul>\n<li>\n<table>\n<thead>\n<tr><th>foo</th><th>bar</th></tr>\n</thead>\n<tbody>\n</tbody>\n</table>\n</li>\n</ul>\n"
      it "if table is indented inside ordered list, it's put there" $
        "1. foo | bar\n   ----|----\n"
          ==-> "<ol>\n<li>\n<table>\n<thead>\n<tr><th>foo</th><th>bar</th></tr>\n</thead>\n<tbody>\n</tbody>\n</table>\n</li>\n</ol>\n"
      it "renders a comprehensive table correctly" $
        withFiles "data/table.md" "data/table.html"
    context "multiple parse errors" $ do
      it "they are reported in correct order" $ do
        let s = "Foo `\n\nBar `.\n"
            pe = ueib <> etok '`' <> ecsc
        s
          ~~-> [ err 5 pe,
                 err 13 pe
               ]
      it "invalid headers are skipped properly" $ do
        let s = "#My header\n\nSomething goes __here __.\n"
        s
          ~~-> [ err 1 (utok 'M' <> etok '#' <> ews),
                 err 37 (ueib <> etoks "__" <> eic)
               ]
      describe "every block in a list gets its parse error propagated" $ do
        context "with unordered list" $
          it "works" $ do
            let s = "- *foo\n\n  *bar\n- *baz\n\n  *quux\n"
                e = ueib <> etok '*' <> eic
            s
              ~~-> [ err 6 e,
                     err 14 e,
                     err 21 e,
                     err 30 e
                   ]
        context "with ordered list" $
          it "works" $ do
            let s = "1. *foo\n\n   *bar\n2. *baz\n\n   *quux\n"
                e = ueib <> etok '*' <> eic
            s
              ~~-> [ err 7 e,
                     err 16 e,
                     err 24 e,
                     err 34 e
                   ]
      it "too big start index of ordered list does not prevent validation of inner inlines" $ do
        let s = "1234567890. *something\n1234567891. [\n"
        s
          ~~-> [ errFancy 0 (indexTooBig 1234567890),
                 err 22 (ueib <> etok '*' <> eic),
                 err 36 (ueib <> eic)
               ]
      it "non-consecutive indices in ordered list do not prevent further validation" $ do
        let s = "1. *foo\n3. *bar\n4. *baz\n"
            e = ueib <> etok '*' <> eic
        s
          ~~-> [ err 7 e,
                 errFancy 8 (indexNonCons 3 2),
                 err 15 e,
                 errFancy 16 (indexNonCons 4 3),
                 err 23 e
               ]
    context "given a complete, comprehensive document" $
      it "outputs expected the HTML fragment" $
        withFiles "data/comprehensive.md" "data/comprehensive.html"
  describe "useExtension" $
    it "applies given extension" $ do
      doc <- mkDoc "Here we go."
      toText (MMark.useExtension (append_ext "..") doc)
        `shouldBe` "<p>Here we go...</p>\n"
  describe "useExtensions" $
    it "applies extensions in the right order" $ do
      doc <- mkDoc "Here we go."
      let exts =
            [ append_ext "3",
              append_ext "2",
              append_ext "1"
            ]
      toText (MMark.useExtensions exts doc)
        `shouldBe` "<p>Here we go.123</p>\n"
  describe "runScanner and scanner" $
    it "extracts information from markdown document" $ do
      doc <- mkDoc "Here we go, pals."
      let n = MMark.runScanner doc (length_scan (const True))
      n `shouldBe` 17
  describe "combining of scanners" $
    it "combines scanners" $ do
      doc <- mkDoc "Here we go, pals."
      let scan =
            (,,)
              <$> length_scan (const True)
              <*> length_scan isSpace
              <*> length_scan isPunctuation
          r = MMark.runScanner doc scan
      r `shouldBe` (17, 3, 2)
  describe "projectYaml" $ do
    context "when document does not contain a YAML section" $
      it "returns Nothing" $ do
        doc <- mkDoc "Here we go."
        MMark.projectYaml doc `shouldBe` Nothing
    context "when document contains a YAML section" $ do
      context "when it is valid" $ do
        let r =
              object
                [ "x" .= Number 100,
                  "y" .= Number 200
                ]
        it "returns the YAML section (1)" $ do
          doc <- mkDoc "---\nx: 100\ny: 200\n---\nHere we go."
          MMark.projectYaml doc `shouldBe` Just r
        it "returns the YAML section (2)" $ do
          doc <- mkDoc "---\nx: 100\ny: 200\n---\n\n"
          MMark.projectYaml doc `shouldBe` Just r
      context "when it is invalid" $ do
        let mappingErr =
              fancy . ErrorCustom . YamlParseError $
                "mapping values are not allowed in this context"
        it "signals correct parse error" $
          let s = "---\nx: 100\ny: x:\n---\nHere we go."
           in s ~-> errFancy 15 mappingErr
        it "does not choke and can report more parse errors" $
          let s = "---\nx: 100\ny: x:\n---\nHere we *go."
           in s
                ~~-> [ errFancy 15 mappingErr,
                       err 33 (ueib <> etok '*' <> eic)
                     ]

----------------------------------------------------------------------------
-- Testing extensions

-- | Append given text to all 'Plain' blocks.
append_ext :: Text -> MMark.Extension
append_ext y = Ext.inlineTrans $ \case
  Plain x -> Plain (x <> y)
  other -> other

----------------------------------------------------------------------------
-- Testing scanners

-- | Scan total number of characters satisfying a predicate in all 'Plain'
-- inlines.
length_scan :: (Char -> Bool) -> L.Fold (Ext.Block (NonEmpty Inline)) Int
length_scan p = Ext.scanner 0 $ \n block ->
  getSum $ Sum n <> foldMap (foldMap f) block
  where
    f (Plain txt) = (Sum . T.length) (T.filter p txt)
    f _ = mempty

----------------------------------------------------------------------------
-- For testing with documents loaded externally

-- | Load a complete markdown document from an external file and compare the
-- final HTML rendering with the contents of another file.
withFiles ::
  -- | Markdown document
  FilePath ->
  -- | HTML document containing the correct result
  FilePath ->
  Expectation
withFiles input output = do
  i <- TIO.readFile input
  o <- TIO.readFile output
  i ==-> o

----------------------------------------------------------------------------
-- Helpers

-- | Unexpected end of inline block.
ueib :: ET s
ueib = ulabel "end of inline block"

-- | Expecting end of inline block.
eeib :: ET s
eeib = elabel "end of inline block"

-- | Expecting end of URI.
euri :: ET s
euri = elabel "end of URI"

-- | Expecting inline content.
eic :: ET s
eic = elabel "inline content"

-- | Expecting white space.
ews :: ET s
ews = elabel "white space"

-- | Expecting code span content.
ecsc :: ET s
ecsc = elabel "code span content"

-- | Expecting common URI components.
euric :: ET Text
euric =
  mconcat
    [ etok '#',
      etok '%',
      etok '/',
      etok ':',
      etok '?',
      etok '@',
      elabel "sub-delimiter",
      elabel "unreserved character"
    ]

-- | The error component complaining that the given 'Text' is not in left-
-- or right- flanking position.
nonFlanking :: Text -> EF MMarkErr
nonFlanking = fancy . ErrorCustom . NonFlankingDelimiterRun . NE.fromList . T.unpack

-- | The error component complaining that the given starting index of an
-- ordered list is too big.
indexTooBig :: Word -> EF MMarkErr
indexTooBig = fancy . ErrorCustom . ListStartIndexTooBig

-- | The error component complaining about non-consecutive indices in an
-- ordered list.
indexNonCons :: Word -> Word -> EF MMarkErr
indexNonCons actual expected =
  fancy . ErrorCustom $
    ListIndexOutOfOrder actual expected

-- | The error component complaining about a missing link\/image reference.
couldNotMatchRef :: Text -> [Text] -> EF MMarkErr
couldNotMatchRef name names =
  fancy . ErrorCustom $
    CouldNotFindReferenceDefinition name names

-- | The error component complaining about a duplicate reference definition.
duplicateRef :: Text -> EF MMarkErr
duplicateRef = fancy . ErrorCustom . DuplicateReferenceDefinition

-- | The error component complaining about an invalid numeric character.
invalidNumChar :: Int -> EF MMarkErr
invalidNumChar = fancy . ErrorCustom . InvalidNumericCharacter

-- | The error component complaining about an unknown HTML5 entity name.
unknownEntity :: Text -> EF MMarkErr
unknownEntity = fancy . ErrorCustom . UnknownHtmlEntityName
