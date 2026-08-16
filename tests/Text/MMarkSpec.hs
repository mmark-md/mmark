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
-- the CommonMark specification. We use the version 0.31.2 (2024-01-28),
-- which can be found online here:
--
-- <https://spec.commonmark.org/0.31.2/>

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
    context "2.4 Backslash escapes" $ do
      it "CM12" $
        "\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~\n"
          ==-> "<p>!&quot;#$%&amp;&#39;()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>\n"
      it "CM13" $
        "\\\t\\A\\a\\ \\3\\φ\\«"
          ==-> "<p>\\\t\\A\\a\\ \\3\\φ\\«</p>\n"
      it "CM14" $
        "\\*not emphasized\\*\n\\<br/> not a tag\n\\[not a link\\](/foo)\n\\`not code\\`\n1\\. not a list\n\\* not a list\n\\# not a heading\n\\[foo\\]: /url \"not a reference\"\n\\&ouml; not a character entity\n"
          ==-> "<p>*not emphasized*\n&lt;br/&gt; not a tag\n[not a link](/foo)\n`not code`\n1. not a list\n* not a list\n# not a heading\n[foo]: /url &quot;not a reference&quot;\n&amp;ouml; not a character entity</p>\n"
      it "CM15" $
        "\\\\*emphasis*" ==-> "<p>\\<em>emphasis</em></p>\n"
      it "CM16" $
        "foo\\\nbar"
          ==-> "<p>foo<br>\nbar</p>\n"
      it "CM17" $
        "`` \\[\\` ``"
          ==-> "<p><code>\\[\\`</code></p>\n"
      it "CM18" $
        "    \\[\\]"
          ==-> "<pre><code>\\[\\]\n</code></pre>\n"
      it "CM19" $
        "~~~\n\\[\\]\n~~~"
          ==-> "<pre><code>\\[\\]\n</code></pre>\n"
      it "CM20" $
        "<https://example.com?find=*>"
          ==-> "<p><a href=\"https://example.com?find=*\">https://example.com?find=*</a></p>\n"
      it "CM21" $
        "<a href=\"/bar\\/)\">"
          ==-> "<p>&lt;a href=&quot;/bar/)&quot;&gt;</p>\n"
      it "CM22" $
        let s = "[foo](/bar\\* \"ti\\*tle\")"
         in s ~-> err 10 (utok '\\' <> euric <> euri)
      it "CM23" $
        let s = "[foo]\n\n[foo]: /bar\\* \"ti\\*tle\""
         in s
              ~~-> [ errFancy 1 (couldNotMatchRef "foo" []),
                     err 18 (utok '\\' <> euric <> euri)
                   ]
      it "CM24" $
        "``` foo\\+bar\nfoo\n```"
          ==-> "<pre><code class=\"language-foo+bar\">foo\n</code></pre>\n"
    context "2.5 Entity and numeric character references" $ do
      it "CM25" $
        "&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;"
          ==-> "<p>  &amp; © Æ Ď\n¾ ℋ ⅆ\n∲ ≧̸</p>\n"
      it "CM26a" $
        "&#35; &#1234; &#992;"
          ==-> "<p># Ӓ Ϡ</p>\n"
      it "CM26b" $
        "&#98765432;" ~-> errFancy 0 (invalidNumChar 98765432)
      it "CM26c" $
        "&#0;" ~-> errFancy 0 (invalidNumChar 0)
      it "CM27" $
        "&#X22; &#XD06; &#xcab;"
          ==-> "<p>&quot; ആ ಫ</p>\n"
      it "CM28a" $
        "&nbsp" ==-> "<p>&amp;nbsp</p>\n"
      it "CM28b" $
        let s = "&x;"
         in s ~-> errFancy 0 (unknownEntity "x")
      it "CM28c" $
        let s = "&#;"
         in s ~-> err 2 (utok ';' <> etok 'x' <> etok 'X' <> elabel "integer")
      it "CM28d" $
        let s = "&#x;"
         in s ~-> err 3 (utok ';' <> elabel "hexadecimal integer")
      it "CM28e" $
        let s = "&ThisIsNotDefined;"
         in s ~-> errFancy 0 (unknownEntity "ThisIsNotDefined")
      it "CM28f" $
        "&hi?;" ==-> "<p>&amp;hi?;</p>\n"
      it "CM29" $
        "&copy"
          ==-> "<p>&amp;copy</p>\n"
      it "CM30" $
        let s = "&MadeUpEntity;"
         in s ~-> errFancy 0 (unknownEntity "MadeUpEntity")
      it "CM31" $
        "<a href=\"&ouml;&ouml;.html\">"
          ==-> "<p>&lt;a href=&quot;\246\246.html&quot;&gt;</p>\n"
      it "CM32" $
        "[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")"
          ##-> p_ (a_ [href_ "/f%26ouml%3b%26ouml%3b", title_ "f\246\246"] "foo")
      it "CM33" $
        "[foo]\n\n[foo]: /f&ouml;&ouml; \"f&ouml;&ouml;\""
          ##-> p_ (a_ [href_ "/f%26ouml%3b%26ouml%3b", title_ "f\246\246"] "foo")
      it "CM34" $
        "``` f&ouml;&ouml;\nfoo\n```"
          ==-> "<pre><code class=\"language-f\246\246\">foo\n</code></pre>\n"
      it "CM35" $
        "`f&ouml;&ouml;`"
          ==-> "<p><code>f&amp;ouml;&amp;ouml;</code></p>\n"
      it "CM36" $
        "    f&ouml;f&ouml;"
          ==-> "<pre><code>f&amp;ouml;f&amp;ouml;\n</code></pre>\n"
      it "CM37" $
        "&#42;foo&#42;\n*foo*\n"
          ==-> "<p>*foo*\n<em>foo</em></p>\n"
      it "CM38" $
        "&#42; foo\n\n* foo\n"
          ==-> "<p>* foo</p>\n<ul>\n<li>\nfoo\n</li>\n</ul>\n"
      it "CM39" $
        "foo&#10;&#10;bar\n" ==-> "<p>foo\n\nbar</p>\n"
      it "CM40" $
        "&#9;foo\n" ==-> "<p>\tfoo</p>\n"
      it "CM41" $
        let s = "[a](url &quot;tit&quot;)\n"
         in s ~-> err 8 (utok '&' <> etok '"' <> etok '\'' <> etok '(' <> etok ')' <> ews)
    context "3.1 Precedence"
      $ it "CM42"
      $ let s = "- `one\n- two`"
         in s
              ~~-> [ err 6 (ueib <> etok '`' <> ecsc),
                     err 13 (ueib <> etok '`' <> ecsc)
                   ]
    context "4.1 Thematic breaks" $ do
      it "CM43" $
        "***\n---\n___" ==-> "<hr>\n<hr>\n<hr>\n"
      it "CM44" $
        "+++" ==-> "<p>+++</p>\n"
      it "CM45" $
        "===" ==-> "<p>===</p>\n"
      it "CM46" $
        let s = "--\n**\n__\n"
         in s ~-> errFancy 3 (nonFlanking "**")
      it "CM47" $
        " ***\n  ***\n   ***" ==-> "<hr>\n<hr>\n<hr>\n"
      it "CM48" $
        "    ***" ==-> "<pre><code>***\n</code></pre>\n"
      it "CM49" $
        let s = "Foo\n    ***\n"
         in s ~-> errFancy 8 (nonFlanking "***")
      it "CM50" $
        "_____________________________________"
          ==-> "<hr>\n"
      it "CM51" $
        " - - -" ==-> "<hr>\n"
      it "CM52" $
        " **  * ** * ** * **" ==-> "<hr>\n"
      it "CM53" $
        "-     -      -      -" ==-> "<hr>\n"
      it "CM54" $
        "- - - -    " ==-> "<hr>\n"
      it "CM55" $
        let s = "_ _ _ _ a\n\na------\n\n---a---\n"
         in s ~-> errFancy 0 (nonFlanking "_")
      it "CM56" $
        " *-*" ==-> "<p><em>-</em></p>\n"
      it "CM57" $
        "- foo\n***\n- bar"
          ==-> "<ul>\n<li>\nfoo\n</li>\n</ul>\n<hr>\n<ul>\n<li>\nbar\n</li>\n</ul>\n"
      it "CM58" $
        "Foo\n***\nbar"
          ==-> "<p>Foo</p>\n<hr>\n<p>bar</p>\n"
      it "CM59" $
        "Foo\n---\nbar"
          ==-> "<p>Foo</p>\n<hr>\n<p>bar</p>\n"
      it "CM60" $
        "* Foo\n* * *\n* Bar"
          ==-> "<ul>\n<li>\nFoo\n</li>\n<li>\n<ul>\n<li>\n<ul>\n<li>\n\n</li>\n</ul>\n</li>\n</ul>\n</li>\n<li>\nBar\n</li>\n</ul>\n"
      it "CM61" $
        "- Foo\n- * * *"
          ==-> "<ul>\n<li>\nFoo\n</li>\n<li>\n<hr>\n</li>\n</ul>\n"
    context "4.2 ATX headings" $ do
      it "CM62" $
        "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo"
          ==-> "<h1 id=\"foo\">foo</h1>\n<h2 id=\"foo\">foo</h2>\n<h3 id=\"foo\">foo</h3>\n<h4 id=\"foo\">foo</h4>\n<h5 id=\"foo\">foo</h5>\n<h6 id=\"foo\">foo</h6>\n"
      it "CM63" $
        let s = "####### foo"
         in s ~-> err 6 (utok '#' <> ews)
      it "CM64" $
        let s = "#5 bolt\n\n#hashtag"
         in s
              ~~-> [ err 1 (utok '5' <> etok '#' <> ews),
                     err 10 (utok 'h' <> etok '#' <> ews)
                   ]
      it "CM65" $
        "\\## foo" ==-> "<p>## foo</p>\n"
      it "CM66" $
        "# foo *bar* \\*baz\\*" ==-> "<h1 id=\"foo-bar-baz\">foo <em>bar</em> *baz*</h1>\n"
      it "CM67" $
        "#                  foo                     "
          ==-> "<h1 id=\"foo\">foo</h1>\n"
      it "CM68" $
        " ### foo\n  ## foo\n   # foo"
          ==-> "<h3 id=\"foo\">foo</h3>\n<h2 id=\"foo\">foo</h2>\n<h1 id=\"foo\">foo</h1>\n"
      it "CM69" $
        "    # foo" ==-> "<pre><code># foo\n</code></pre>\n"
      it "CM70" $
        "foo\n    # bar" ==-> "<p>foo\n# bar</p>\n"
      it "CM71" $
        "## foo ##\n  ###   bar    ###"
          ==-> "<h2 id=\"foo\">foo</h2>\n<h3 id=\"bar\">bar</h3>\n"
      it "CM72" $
        "# foo ##################################\n##### foo ##"
          ==-> "<h1 id=\"foo\">foo</h1>\n<h5 id=\"foo\">foo</h5>\n"
      it "CM73" $
        "### foo ###     " ==-> "<h3 id=\"foo\">foo</h3>\n"
      it "CM74" $
        "### foo ### b" ==-> "<h3 id=\"foo-b\">foo ### b</h3>\n"
      it "CM75" $
        "# foo#" ==-> "<h1 id=\"foo\">foo#</h1>\n"
      it "CM76" $
        "### foo \\###\n## foo #\\##\n# foo \\#"
          ==-> "<h3 id=\"foo\">foo ###</h3>\n<h2 id=\"foo\">foo ###</h2>\n<h1 id=\"foo\">foo #</h1>\n"
      it "CM77" $
        "****\n## foo\n****"
          ==-> "<hr>\n<h2 id=\"foo\">foo</h2>\n<hr>\n"
      it "CM78" $
        "Foo bar\n# baz\nBar foo"
          ==-> "<p>Foo bar</p>\n<h1 id=\"baz\">baz</h1>\n<p>Bar foo</p>\n"
      it "CM79" $
        let s = "## \n#\n### ###"
         in s
              ~~-> [ err 3 (utok '\n' <> elabel "heading character" <> ews),
                     err 5 (utok '\n' <> etok '#' <> ews)
                   ]
    context "4.3 Setext headings" $ do
      -- NOTE we do not support them, the tests have been adjusted
      -- accordingly.
      it "CM80" $
        "Foo *bar*\n=========\n\nFoo *bar*\n---------"
          ==-> "<p>Foo <em>bar</em>\n=========</p>\n<p>Foo <em>bar</em></p>\n<hr>\n"
      it "CM81" $
        "Foo *bar\nbaz*\n===="
          ==-> "<p>Foo <em>bar\nbaz</em>\n====</p>\n"
      it "CM82" $
        "  Foo *bar\nbaz*\t\n====\n"
          ==-> "<p>Foo <em>bar\nbaz</em>\n====</p>\n"
      it "CM83" $
        "Foo\n-------------------------\n\nFoo\n="
          ==-> "<p>Foo</p>\n<hr>\n<p>Foo\n=</p>\n"
      it "CM84" $
        "   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ==="
          ==-> "<p>Foo</p>\n<hr>\n<p>Foo</p>\n<hr>\n<p>Foo\n===</p>\n"
      it "CM85" $
        "    Foo\n    ---\n\n    Foo\n---"
          ==-> "<pre><code>Foo\n---\n\nFoo\n</code></pre>\n<hr>\n"
      it "CM86" $
        "Foo\n   ----      "
          ==-> "<p>Foo</p>\n<hr>\n"
      it "CM87" $
        "Foo\n    ---"
          ==-> "<p>Foo\n---</p>\n"
      it "CM88" $
        "Foo\n= =\n\nFoo\n--- -"
          ==-> "<p>Foo\n= =</p>\n<p>Foo</p>\n<hr>\n"
      it "CM89" $
        "Foo  \n-----"
          ==-> "<p>Foo</p>\n<hr>\n"
      it "CM90" $
        "Foo\\\n----"
          ==-> "<p>Foo\\</p>\n<hr>\n"
      it "CM91" $
        let s = "`Foo\n----\n`\n\n<a title=\"a lot\n---\nof dashes\"/>\n"
         in s
              ~~-> [ err 4 (ueib <> etok '`' <> ecsc),
                     err 11 (ueib <> etok '`' <> ecsc)
                   ]
      it "CM92" $
        "> Foo\n---"
          ==-> "<blockquote>\n<p>Foo</p>\n</blockquote>\n<hr>\n"
      it "CM93" $
        "> foo\nbar\n==="
          ==-> "<blockquote>\n<p>foo\nbar\n===</p>\n</blockquote>\n"
      it "CM94" $
        "- Foo\n---"
          ==-> "<ul>\n<li>\nFoo\n</li>\n</ul>\n<hr>\n"
      it "CM95" $
        "Foo\nBar\n---"
          ==-> "<p>Foo\nBar</p>\n<hr>\n"
      it "CM96" $
        "---\nFoo\n---\nBar\n---\nBaz"
          ==-> "<p>Bar</p>\n<hr>\n<p>Baz</p>\n"
      it "CM97" $
        "\n===="
          ==-> "<p>====</p>\n"
      it "CM98" $
        "---\n---"
          ==-> "" -- thinks that it's got a YAML block
      it "CM99" $
        "- foo\n-----"
          ==-> "<ul>\n<li>\nfoo\n</li>\n</ul>\n<hr>\n"
      it "CM100" $
        "    foo\n---"
          ==-> "<pre><code>foo\n</code></pre>\n<hr>\n"
      it "CM101" $
        "> foo\n-----"
          ==-> "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr>\n"
      it "CM102" $
        "\\> foo\n------"
          ==-> "<p>&gt; foo</p>\n<hr>\n"
      it "CM103" $
        "Foo\n\nbar\n---\nbaz"
          ==-> "<p>Foo</p>\n<p>bar</p>\n<hr>\n<p>baz</p>\n"
      it "CM104" $
        "Foo\nbar\n\n---\n\nbaz"
          ==-> "<p>Foo\nbar</p>\n<hr>\n<p>baz</p>\n"
      it "CM105" $
        "Foo\nbar\n* * *\nbaz"
          ==-> "<p>Foo\nbar</p>\n<hr>\n<p>baz</p>\n"
      it "CM106" $
        "Foo\nbar\n\\---\nbaz"
          ==-> "<p>Foo\nbar\n---\nbaz</p>\n"
    context "4.4 Indented code blocks" $ do
      it "CM107" $
        "    a simple\n      indented code block"
          ==-> "<pre><code>a simple\n  indented code block\n</code></pre>\n"
      it "CM108" $
        "  - foo\n\n    bar"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      it "CM109" $
        "1.  foo\n\n    - bar"
          ==-> "<ol>\n<li>\n<p>foo</p>\n<ul>\n<li>\nbar\n</li>\n</ul>\n</li>\n</ol>\n"
      it "CM110" $
        "    <a/>\n    *hi*\n\n    - one"
          ==-> "<pre><code>&lt;a/&gt;\n*hi*\n\n- one\n</code></pre>\n"
      it "CM111" $
        "    chunk1\n\n    chunk2\n  \n \n \n    chunk3"
          ==-> "<pre><code>chunk1\n\nchunk2\n\n\n\nchunk3\n</code></pre>\n"
      it "CM112" $
        "    chunk1\n      \n      chunk2"
          ==-> "<pre><code>chunk1\n  \n  chunk2\n</code></pre>\n"
      it "CM113" $
        "Foo\n    bar\n"
          ==-> "<p>Foo\nbar</p>\n"
      it "CM114" $
        "    foo\nbar"
          ==-> "<pre><code>foo\n</code></pre>\n<p>bar</p>\n"
      it "CM115" $
        "# Heading\n    foo\nHeading\n------\n    foo\n----\n"
          ==-> "<h1 id=\"heading\">Heading</h1>\n<pre><code>foo\n</code></pre>\n<p>Heading</p>\n<hr>\n<pre><code>foo\n</code></pre>\n<hr>\n"
      it "CM116" $
        "        foo\n    bar"
          ==-> "<pre><code>    foo\nbar\n</code></pre>\n"
      it "CM117" $
        "\n    \n    foo\n    \n"
          ==-> "<pre><code>foo\n</code></pre>\n"
      it "CM118" $
        "    foo  "
          ==-> "<pre><code>foo  \n</code></pre>\n"
    context "4.5 Fenced code blocks" $ do
      it "CM119" $
        "```\n<\n >\n```"
          ==-> "<pre><code>&lt;\n &gt;\n</code></pre>\n"
      it "CM120" $
        "~~~\n<\n >\n~~~"
          ==-> "<pre><code>&lt;\n &gt;\n</code></pre>\n"
      it "CM121" $
        "``\nfoo\n``\n"
          ==-> "<p><code>foo</code></p>\n"
      it "CM122" $
        "```\naaa\n~~~\n```"
          ==-> "<pre><code>aaa\n~~~\n</code></pre>\n"
      it "CM123" $
        "~~~\naaa\n```\n~~~"
          ==-> "<pre><code>aaa\n```\n</code></pre>\n"
      it "CM124" $
        "````\naaa\n```\n``````"
          ==-> "<pre><code>aaa\n```\n</code></pre>\n"
      it "CM125" $
        "~~~~\naaa\n~~~\n~~~~"
          ==-> "<pre><code>aaa\n~~~\n</code></pre>\n"
      it "CM126" $
        let s = "```"
         in s ~-> err 3 (ueib <> etok '`' <> ecsc)
      it "CM127" $
        let s = "`````\n\n```\naaa\n"
         in s
              ~-> err
                15
                (ueof <> elabel "closing code fence" <> elabel "code block content")
      -- NOTE CommonMark closes the code fence implicitly when the block
      -- quote containing it ends, while MMark requires an explicit closing
      -- fence, see CM126, CM127, CM137, and CM139. The block quote ends at the
      -- blank line, which is where we report the missing fence.
      it "CM128" $
        let s = "> ```\n> aaa\n\nbbb\n"
         in s ~-> err 12 (ebqm <> eccf <> ecbc)
      it "CM129" $
        "```\n\n  \n```"
          ==-> "<pre><code>\n  \n</code></pre>\n"
      it "CM130" $
        "```\n```"
          ==-> "<pre><code></code></pre>\n"
      it "CM131" $
        " ```\n aaa\naaa\n```"
          ==-> "<pre><code>aaa\naaa\n</code></pre>\n"
      it "CM132" $
        "  ```\naaa\n  aaa\naaa\n  ```"
          ==-> "<pre><code>aaa\naaa\naaa\n</code></pre>\n"
      it "CM133" $
        "   ```\n   aaa\n    aaa\n  aaa\n   ```"
          ==-> "<pre><code>aaa\n aaa\naaa\n</code></pre>\n"
      it "CM134" $
        "    ```\n    aaa\n    ```"
          ==-> "<pre><code>```\naaa\n```\n</code></pre>\n"
      it "CM135" $
        "```\naaa\n  ```"
          ==-> "<pre><code>aaa\n</code></pre>\n"
      it "CM136" $
        "   ```\naaa\n  ```"
          ==-> "<pre><code>aaa\n</code></pre>\n"
      it "CM137" $
        let s = "```\naaa\n    ```\n"
         in s
              ~-> err
                16
                (ueof <> elabel "closing code fence" <> elabel "code block content")
      it "CM138" $
        "``` ```\naaa"
          ==-> "<p><code> </code>\naaa</p>\n"
      it "CM139" $
        let s = "~~~~~~\naaa\n~~~ ~~\n"
         in s
              ~-> err
                18
                (ueof <> elabel "closing code fence" <> elabel "code block content")
      it "CM140" $
        "foo\n```\nbar\n```\nbaz"
          ==-> "<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n"
      it "CM141" $
        "foo\n---\n~~~\nbar\n~~~\n# baz"
          ==-> "<p>foo</p>\n<hr>\n<pre><code>bar\n</code></pre>\n<h1 id=\"baz\">baz</h1>\n"
      it "CM142" $
        "```ruby\ndef foo(x)\n  return 3\nend\n```"
          ==-> "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
      it "CM143" $
        "~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~"
          ==-> "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
      it "CM144" $
        "````;\n````"
          ==-> "<pre><code class=\"language-;\"></code></pre>\n"
      it "CM145" $
        "``` aa ```\nfoo"
          ==-> "<p><code>aa</code>\nfoo</p>\n"
      it "CM146" $
        "~~~ aa ``` ~~~\nfoo\n~~~\n"
          ==-> "<pre><code class=\"language-aa\">foo\n</code></pre>\n"
      it "CM147" $
        "```\n``` aaa\n```"
          ==-> "<pre><code>``` aaa\n</code></pre>\n"
    context "4.6 HTML blocks" $
      -- NOTE We do not support HTML blocks, see the readme.
      return ()
    context "4.7 Link reference definitions" $ do
      it "CM192" $
        "[foo]: /url \"title\"\n\n[foo]" ##-> p_ (a_ [href_ "/url", title_ "title"] "foo")
      it "CM193" $
        "   [foo]: \n      /url  \n           'the title'  \n\n[foo]"
          ##-> p_ (a_ [href_ "/url", title_ "the title"] "foo")
      it "CM194" $
        let s = "[Foo bar\\]]:my_(url) 'title (with parens)'\n\n[Foo bar\\]]"
         in s
              ~~-> [ err 19 (utoks ") " <> euric <> elabel "newline" <> ews),
                     errFancy 45 (couldNotMatchRef "Foo bar]" [])
                   ]
      it "CM195" $
        "[Foo bar]:\n<my%20url>\n'title'\n\n[Foo bar]"
          ##-> p_ (a_ [href_ "my%20url", title_ "title"] "Foo bar")
      it "CM196" $
        "[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]"
          ##-> p_ (a_ [href_ "/url", title_ "\ntitle\nline1\nline2\n"] "foo")
      it "CM197" $
        "[foo]: /url 'title\n\nwith blank line'\n\n[foo]"
          ##-> p_ (a_ [href_ "/url", title_ "title\n\nwith blank line"] "foo")
      it "CM198" $
        "[foo]:\n/url\n\n[foo]"
          ==-> "<p><a href=\"/url\">foo</a></p>\n"
      it "CM199" $
        let s = "[foo]:\n\n[foo]"
         in s
              ~~-> [ err 7 (utok '\n' <> etok '<' <> elabel "URI" <> ews),
                     errFancy 9 (couldNotMatchRef "foo" [])
                   ]
      it "CM200" $
        "[foo]: <>\n\n[foo]\n" ==-> "<p><a href>foo</a></p>\n"
      it "CM201" $
        "[foo]: <bar>(baz)\n\n[foo]\n"
          ~~-> [ err 12 (utoks "(b" <> elabel "newline" <> ews),
                 errFancy 20 (couldNotMatchRef "foo" [])
               ]
      it "CM202" $
        let s = "[foo]: /url\\bar\\*baz \"foo\\\"bar\\baz\"\n\n[foo]\n"
         in s ~-> err 11 (utok '\\' <> euric <> euri)
      it "CM203" $
        "[foo]\n\n[foo]: url"
          ==-> "<p><a href=\"url\">foo</a></p>\n"
      it "CM204" $
        let s = "[foo]\n\n[foo]: first\n[foo]: second\n"
         in s ~-> errFancy 21 (duplicateRef "foo")
      it "CM205" $
        "[FOO]: /url\n\n[Foo]"
          ==-> "<p><a href=\"/url\">Foo</a></p>\n"
      it "CM206" $
        "[ΑΓΩ]: /%CF%86%CE%BF%CF%85\n\n[αγω]"
          ==-> "<p><a href=\"/%cf%86%ce%bf%cf%85\">αγω</a></p>\n"
      it "CM207" $
        "[foo]: /url"
          ==-> ""
      it "CM208" $
        "[\nfoo\n]: /url\nbar"
          ==-> "<p>bar</p>\n"
      it "CM209" $
        let s = "[foo]: /url \"title\" ok"
         in s ~-> err 20 (utoks "ok" <> elabel "newline" <> ews)
      it "CM210" $
        let s = "[foo]: /url\n\"title\" ok\n"
         in s ~-> err 20 (utoks "ok" <> elabel "newline" <> ews)
      it "CM211" $
        "    [foo]: /url \"title\""
          ==-> "<pre><code>[foo]: /url &quot;title&quot;\n</code></pre>\n"
      it "CM212" $
        "```\n[foo]: /url\n```"
          ==-> "<pre><code>[foo]: /url\n</code></pre>\n"
      it "CM213" $
        let s = "Foo\n[bar]: /baz\n\n[bar]\n"
         in s
              ~~-> [ errFancy 5 (couldNotMatchRef "bar" []),
                     errFancy 18 (couldNotMatchRef "bar" [])
                   ]
      it "CM214" $
        "# [Foo]\n[foo]: /url\n> bar"
          ==-> "<h1 id=\"foo\"><a href=\"/url\">Foo</a></h1>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
      it "CM215" $
        "[foo]: /url\nbar\n===\n[foo]\n"
          ==-> "<p>bar\n===\n<a href=\"/url\">foo</a></p>\n"
      it "CM216" $
        "[foo]: /url\n===\n[foo]\n"
          ==-> "<p>===\n<a href=\"/url\">foo</a></p>\n"
      it "CM217" $
        "[foo]: /foo-url \"foo\"\n[bar]: /bar-url\n  \"bar\"\n[baz]: /baz-url\n\n[foo],\n[bar],\n[baz]"
          ##-> p_
            ( do
                a_ [href_ "/foo-url", title_ "foo"] "foo"
                ",\n"
                a_ [href_ "/bar-url", title_ "bar"] "bar"
                ",\n"
                a_ [href_ "/baz-url"] "baz"
            )
      it "CM218" $
        "[foo]\n\n> [foo]: /url"
          ==-> "<p><a href=\"/url\">foo</a></p>\n<blockquote>\n</blockquote>\n"
    context "4.8 Paragraphs" $ do
      it "CM219" $
        "aaa\n\nbbb"
          ==-> "<p>aaa</p>\n<p>bbb</p>\n"
      it "CM220" $
        "aaa\nbbb\n\nccc\nddd"
          ==-> "<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n"
      it "CM221" $
        "aaa\n\n\nbbb"
          ==-> "<p>aaa</p>\n<p>bbb</p>\n"
      it "CM222" $
        "  aaa\n bbb"
          ==-> "<p>aaa\nbbb</p>\n"
      it "CM223" $
        "aaa\n             bbb\n                                       ccc"
          ==-> "<p>aaa\nbbb\nccc</p>\n"
      it "CM224" $
        "   aaa\nbbb" ==-> "<p>aaa\nbbb</p>\n"
      it "CM225" $
        "    aaa\nbbb"
          ==-> "<pre><code>aaa\n</code></pre>\n<p>bbb</p>\n"
      it "CM226" $
        "aaa     \nbbb     "
          ==-> "<p>aaa\nbbb</p>\n"
    context "4.9 Blank lines"
      $ it "CM227"
      $ "  \n\naaa\n  \n\n# aaa\n\n  "
        ==-> "<p>aaa</p>\n<h1 id=\"aaa\">aaa</h1>\n"
    context "5.1 Block quotes" $ do
      it "CM228" $
        "> # Foo\n> bar\n> baz"
          ==-> "<blockquote>\n<h1 id=\"foo\">Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM229" $
        "># Foo\n>bar\n> baz"
          ==-> "<blockquote>\n<h1 id=\"foo\">Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM230" $
        "   > # Foo\n   > bar\n > baz"
          ==-> "<blockquote>\n<h1 id=\"foo\">Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM231" $
        "    > # Foo\n    > bar\n    > baz"
          ==-> "<pre><code>&gt; # Foo\n&gt; bar\n&gt; baz\n</code></pre>\n"
      it "CM232" $
        "> # Foo\n> bar\nbaz"
          ==-> "<blockquote>\n<h1 id=\"foo\">Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM233" $
        "> bar\nbaz\n> foo"
          ==-> "<blockquote>\n<p>bar\nbaz\nfoo</p>\n</blockquote>\n"
      it "CM234" $
        "> foo\n---"
          ==-> "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr>\n"
      it "CM235" $
        "> - foo\n- bar"
          ==-> "<blockquote>\n<ul>\n<li>\nfoo\n</li>\n</ul>\n</blockquote>\n<ul>\n<li>\nbar\n</li>\n</ul>\n"
      it "CM236" $
        ">     foo\n    bar"
          ==-> "<blockquote>\n<pre><code>foo\n</code></pre>\n</blockquote>\n<pre><code>bar\n</code></pre>\n"
      -- NOTE Unlike CommonMark, MMark demands that code fences be closed
      -- explicitly, see CM126, CM127, CM137, and CM139. The block quote ends
      -- at the second line, so the fence it opens is never closed, just like
      -- in CM128.
      it "CM237" $
        let s = "> ```\nfoo\n```"
         in s ~-> err 6 (ebqm <> eccf <> ecbc)
      it "CM238" $
        "> foo\n    - bar"
          ==-> "<blockquote>\n<p>foo\n- bar</p>\n</blockquote>\n"
      it "CM239" $
        ">"
          ==-> "<blockquote>\n</blockquote>\n"
      it "CM240" $
        ">\n>  \n> "
          ==-> "<blockquote>\n</blockquote>\n"
      it "CM241" $
        ">\n> foo\n>  "
          ==-> "<blockquote>\n<p>foo</p>\n</blockquote>\n"
      it "CM242" $
        "> foo\n\n> bar"
          ==-> "<blockquote>\n<p>foo</p>\n</blockquote>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
      it "CM243" $
        "> foo\n> bar"
          ==-> "<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n"
      it "CM244" $
        "> foo\n>\n> bar"
          ==-> "<blockquote>\n<p>foo</p>\n<p>bar</p>\n</blockquote>\n"
      it "CM245" $
        "foo\n> bar"
          ==-> "<p>foo</p>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
      it "CM246" $
        "> aaa\n***\n> bbb"
          ==-> "<blockquote>\n<p>aaa</p>\n</blockquote>\n<hr>\n<blockquote>\n<p>bbb</p>\n</blockquote>\n"
      it "CM247" $
        "> bar\nbaz"
          ==-> "<blockquote>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM248" $
        "> bar\n\nbaz"
          ==-> "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
      it "CM249" $
        "> bar\n>\nbaz"
          ==-> "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
      it "CM250" $
        "> > > foo\nbar"
          ==-> "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"
      it "CM251" $
        ">>> foo\n> bar\n>>baz"
          ==-> "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar\nbaz</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"
      it "CM252" $
        ">     code\n\n>    not code"
          ==-> "<blockquote>\n<pre><code>code\n</code></pre>\n</blockquote>\n<blockquote>\n<p>not code</p>\n</blockquote>\n"
    context "5.2 List items" $ do
      it "CM253" $
        "A paragraph\nwith two lines.\n\n    indented code\n\n> A block quote."
          ==-> "<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n"
      it "CM254" $
        "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote."
          ==-> "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM255" $
        "- one\n\n two"
          ==-> "<ul>\n<li>\none\n</li>\n</ul>\n<p>two</p>\n"
      it "CM256" $
        "- one\n\n  two"
          ==-> "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"
      it "CM257" $
        " -    one\n\n     two"
          ==-> "<ul>\n<li>\none\n</li>\n</ul>\n<pre><code> two\n</code></pre>\n"
      it "CM258" $
        " -    one\n\n      two"
          ==-> "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"
      it "CM259" $
        "   > > 1.  one\n>>\n>>     two"
          ==-> "<blockquote>\n<blockquote>\n<ol>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ol>\n</blockquote>\n</blockquote>\n"
      it "CM260" $
        ">>- one\n>>\n  >  > two"
          ==-> "<blockquote>\n<blockquote>\n<ul>\n<li>\none\n</li>\n</ul>\n<p>two</p>\n</blockquote>\n</blockquote>\n"
      it "CM261" $
        "-one\n\n2.two"
          ==-> "<p>-one</p>\n<p>2.two</p>\n"
      it "CM262" $
        "- foo\n\n\n  bar"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      it "CM263" $
        "1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam"
          ==-> "<ol>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n<blockquote>\n<p>bam</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM264" $
        "- Foo\n\n      bar\n\n\n      baz"
          ==-> "<ul>\n<li>\n<p>Foo</p>\n<pre><code>bar\n\n\nbaz\n</code></pre>\n</li>\n</ul>\n"
      it "CM265" $
        "123456789. ok"
          ==-> "<ol start=\"123456789\">\n<li>\nok\n</li>\n</ol>\n"
      it "CM266" $
        let s = "1234567890. not ok\n"
         in s ~-> errFancy 0 (indexTooBig 1234567890)
      it "CM267" $
        "0. ok"
          ==-> "<ol start=\"0\">\n<li>\nok\n</li>\n</ol>\n"
      it "CM268" $
        "003. ok"
          ==-> "<ol start=\"3\">\n<li>\nok\n</li>\n</ol>\n"
      it "CM269" $
        "-1. not ok"
          ==-> "<p>-1. not ok</p>\n"
      it "CM270" $
        "- foo\n\n      bar"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ul>\n"
      it "CM271" $
        "  10.  foo\n\n           bar"
          ==-> "<ol start=\"10\">\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ol>\n"
      it "CM272" $
        "    indented code\n\nparagraph\n\n    more code"
          ==-> "<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n"
      it "CM273" $
        "1.     indented code\n\n   paragraph\n\n       more code"
          ==-> "<ol>\n<li>\n<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>\n"
      it "CM274" $
        "1.      indented code\n\n   paragraph\n\n       more code"
          ==-> "<ol>\n<li>\n<pre><code> indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>\n"
      it "CM275" $
        "   foo\n\nbar"
          ==-> "<p>foo</p>\n<p>bar</p>\n"
      it "CM276" $
        "-    foo\n\n  bar"
          ==-> "<ul>\n<li>\nfoo\n</li>\n</ul>\n<p>bar</p>\n"
      it "CM277" $
        "-  foo\n\n   bar"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      it "CM278" $
        "-\n  foo\n-\n  ```\n  bar\n  ```\n-\n      baz"
          ==-> "<ul>\n<li>\n<p>foo</p>\n</li>\n<li>\n<pre><code>bar\n</code></pre>\n</li>\n<li>\n<pre><code>baz\n</code></pre>\n</li>\n</ul>\n"
      it "CM279" $
        "-   \n  foo"
          ==-> "<ul>\n<li>\nfoo\n</li>\n</ul>\n"
      it "CM280a" $
        "-\n\n  foo"
          ==-> "<ul>\n<li>\n\n</li>\n</ul>\n<p>foo</p>\n"
      it "CM280b" $
        "1.\n\n   foo"
          ==-> "<ol>\n<li>\n\n</li>\n</ol>\n<p>foo</p>\n"
      it "CM281" $
        "- foo\n-\n- bar"
          ==-> "<ul>\n<li>\nfoo\n</li>\n<li>\n\n</li>\n<li>\nbar\n</li>\n</ul>\n"
      it "CM282" $
        "- foo\n-   \n- bar"
          ==-> "<ul>\n<li>\nfoo\n</li>\n<li>\n\n</li>\n<li>\nbar\n</li>\n</ul>\n"
      it "CM283" $
        "1. foo\n2.\n3. bar"
          ==-> "<ol>\n<li>\nfoo\n</li>\n<li>\n\n</li>\n<li>\nbar\n</li>\n</ol>\n"
      it "CM284" $
        "*"
          ==-> "<ul>\n<li>\n\n</li>\n</ul>\n"
      it "CM285" $
        "foo\n*\n\nfoo\n1."
          ==-> "<p>foo</p>\n<ul>\n<li>\n\n</li>\n</ul>\n<p>foo</p>\n<ol>\n<li>\n\n</li>\n</ol>\n"
      it "CM286" $
        " 1.  A paragraph\n     with two lines.\n\n         indented code\n\n     > A block quote."
          ==-> "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM287" $
        "  1.  A paragraph\n      with two lines.\n\n          indented code\n\n      > A block quote."
          ==-> "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM288" $
        "   1.  A paragraph\n       with two lines.\n\n           indented code\n\n       > A block quote."
          ==-> "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM289" $
        "    1.  A paragraph\n        with two lines.\n\n            indented code\n\n        > A block quote."
          ==-> "<pre><code>1.  A paragraph\n    with two lines.\n\n        indented code\n\n    &gt; A block quote.\n</code></pre>\n"
      it "CM290" $
        "  1.  A paragraph\nwith two lines.\n\n          indented code\n\n      > A block quote."
          ==-> "<ol>\n<li>\nA paragraph\n</li>\n</ol>\n<p>with two lines.</p>\n<pre><code>      indented code\n\n  &gt; A block quote.\n</code></pre>\n"
      it "CM291" $
        "  1.  A paragraph\n    with two lines."
          ==-> "<ol>\n<li>\nA paragraph\n</li>\n</ol>\n<pre><code>with two lines.\n</code></pre>\n"
      it "CM292" $
        "> 1. > Blockquote\ncontinued here."
          ==-> "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>\n"
      it "CM293" $
        "> 1. > Blockquote\n> continued here."
          ==-> "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>\n"
      it "CM294" $
        "- foo\n  - bar\n    - baz\n      - boo"
          ==-> "<ul>\n<li>\nfoo\n<ul>\n<li>\nbar\n<ul>\n<li>\nbaz\n<ul>\n<li>\nboo\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM295" $
        "- foo\n - bar\n  - baz\n   - boo"
          ==-> "<ul>\n<li>\nfoo\n</li>\n<li>\nbar\n</li>\n<li>\nbaz\n</li>\n<li>\nboo\n</li>\n</ul>\n"
      it "CM296" $
        "10) foo\n    - bar"
          ==-> "<ol start=\"10\">\n<li>\nfoo\n<ul>\n<li>\nbar\n</li>\n</ul>\n</li>\n</ol>\n"
      it "CM297" $
        "10) foo\n   - bar"
          ==-> "<ol start=\"10\">\n<li>\nfoo\n</li>\n</ol>\n<ul>\n<li>\nbar\n</li>\n</ul>\n"
      it "CM298" $
        "- - foo"
          ==-> "<ul>\n<li>\n<ul>\n<li>\nfoo\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM299" $
        "1. - 2. foo"
          ==-> "<ol>\n<li>\n<ul>\n<li>\n<ol start=\"2\">\n<li>\nfoo\n</li>\n</ol>\n</li>\n</ul>\n</li>\n</ol>\n"
      it "CM300" $
        "- # Foo\n- Bar\n  ---\n  baz"
          ==-> "<ul>\n<li>\n<h1 id=\"foo\">Foo</h1>\n</li>\n<li>\n<p>Bar</p>\n<hr>\n<p>baz</p>\n</li>\n</ul>\n"
    context "5.3 Lists" $ do
      it "CM301" $
        "- foo\n- bar\n+ baz"
          ==-> "<ul>\n<li>\nfoo\n</li>\n<li>\nbar\n</li>\n</ul>\n<ul>\n<li>\nbaz\n</li>\n</ul>\n"
      it "CM302" $
        "1. foo\n2. bar\n3) baz"
          ==-> "<ol>\n<li>\nfoo\n</li>\n<li>\nbar\n</li>\n</ol>\n<ol start=\"3\">\n<li>\nbaz\n</li>\n</ol>\n"
      it "CM303" $
        "Foo\n- bar\n- baz"
          ==-> "<p>Foo</p>\n<ul>\n<li>\nbar\n</li>\n<li>\nbaz\n</li>\n</ul>\n"
      it "CM304" $
        "The number of windows in my house is\n14.  The number of doors is 6."
          ==-> "<p>The number of windows in my house is</p>\n<ol start=\"14\">\n<li>\nThe number of doors is 6.\n</li>\n</ol>\n"
      it "CM305" $
        "The number of windows in my house is\n1.  The number of doors is 6."
          ==-> "<p>The number of windows in my house is</p>\n<ol>\n<li>\nThe number of doors is 6.\n</li>\n</ol>\n"
      it "CM306" $
        "- foo\n\n- bar\n\n\n- baz"
          ==-> "<ul>\n<li>\n<p>foo</p>\n</li>\n<li>\n<p>bar</p>\n</li>\n<li>\n<p>baz</p>\n</li>\n</ul>\n"
      it "CM307" $
        "- foo\n  - bar\n    - baz\n\n\n      bim"
          ==-> "<ul>\n<li>\nfoo\n<ul>\n<li>\nbar\n<ul>\n<li>\n<p>baz</p>\n<p>bim</p>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM308" $
        "- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim"
          ==-> "<ul>\n<li>\nfoo\n</li>\n<li>\nbar\n</li>\n</ul>\n<p>&lt;!-- --&gt;</p>\n<ul>\n<li>\nbaz\n</li>\n<li>\nbim\n</li>\n</ul>\n"
      it "CM309" $
        "-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code"
          ==-> "<ul>\n<li>\n<p>foo</p>\n<p>notcode</p>\n</li>\n<li>\n<p>foo</p>\n</li>\n</ul>\n<p>&lt;!-- --&gt;</p>\n<pre><code>code\n</code></pre>\n"
      it "CM310" $
        "- a\n - b\n  - c\n   - d\n  - e\n - f\n- g"
          ==-> "<ul>\n<li>\na\n</li>\n<li>\nb\n</li>\n<li>\nc\n</li>\n<li>\nd\n</li>\n<li>\ne\n</li>\n<li>\nf\n</li>\n<li>\ng\n</li>\n</ul>\n"
      it "CM311" $
        "1. a\n\n  2. b\n\n   3. c\n"
          ==-> "<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ol>\n"
      it "CM312" $
        "- a\n - b\n  - c\n   - d\n    - e\n"
          ==-> "<ul>\n<li>\na\n</li>\n<li>\nb\n</li>\n<li>\nc\n</li>\n<li>\nd\n</li>\n<li>\ne\n</li>\n</ul>\n"
      it "CM313" $
        "1. a\n\n  2. b\n\n    3. c"
          ==-> "<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ol>\n"
      it "CM314" $
        "- a\n- b\n\n- c"
          ==-> "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
      it "CM315" $
        "* a\n*\n\n* c"
          ==-> "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p></p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
      it "CM316" $
        "- a\n- b\n\n  c\n- d"
          ==-> "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
      it "CM317" $
        "- a\n- b\n\n  [ref]: /url\n- d"
          ==-> "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
      it "CM318" $
        "- a\n- ```\n  b\n\n\n  ```\n- c"
          ==-> "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<pre><code>b\n\n\n</code></pre>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
      it "CM319" $
        "- a\n  - b\n\n    c\n- d"
          ==-> "<ul>\n<li>\na\n<ul>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n</ul>\n</li>\n<li>\nd\n</li>\n</ul>\n"
      it "CM320" $
        "* a\n  > b\n  >\n* c"
          ==-> "<ul>\n<li>\n<p>a</p>\n<blockquote>\n<p>b</p>\n</blockquote>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
      it "CM321" $
        "- a\n  > b\n  ```\n  c\n  ```\n- d"
          ==-> "<ul>\n<li>\n<p>a</p>\n<blockquote>\n<p>b</p>\n</blockquote>\n<pre><code>c\n</code></pre>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
      it "CM322" $
        "- a"
          ==-> "<ul>\n<li>\na\n</li>\n</ul>\n"
      it "CM323" $
        "- a\n  - b"
          ==-> "<ul>\n<li>\na\n<ul>\n<li>\nb\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM324" $
        "1. ```\n   foo\n   ```\n\n   bar"
          ==-> "<ol>\n<li>\n<pre><code>foo\n</code></pre>\n<p>bar</p>\n</li>\n</ol>\n"
      it "CM325" $
        "* foo\n  * bar\n\n  baz"
          ==-> "<ul>\n<li>\nfoo\n<ul>\n<li>\nbar\n</li>\n</ul>\nbaz\n</li>\n</ul>\n"
      it "CM326" $
        "- a\n  - b\n  - c\n\n- d\n  - e\n  - f"
          ==-> "<ul>\n<li>\na\n<ul>\n<li>\nb\n</li>\n<li>\nc\n</li>\n</ul>\n</li>\n<li>\nd\n<ul>\n<li>\ne\n</li>\n<li>\nf\n</li>\n</ul>\n</li>\n</ul>\n"
    context "6 Inlines"
      $ it "CM327"
      $ let s = "`hi`lo`\n"
         in s ~-> err 7 (ueib <> etok '`' <> ecsc)
    context "6.1 Code spans" $ do
      it "CM328" $
        "`foo`" ==-> "<p><code>foo</code></p>\n"
      it "CM329" $
        "`` foo ` bar ``"
          ==-> "<p><code>foo ` bar</code></p>\n"
      it "CM330" $
        "` `` `" ==-> "<p><code>``</code></p>\n"
      it "CM331" $
        "`  ``  `\n" ==-> "<p><code> `` </code></p>\n"
      it "CM332" $
        "` a`\n" ==-> "<p><code> a</code></p>\n"
      it "CM333" $
        "` b `" ==-> "<p><code> b </code></p>\n"
      it "CM334" $
        "`\160`\n`  `\n" ==-> "<p><code>\160</code>\n<code>  </code></p>\n"
      it "CM335" $
        "``\nfoo\nbar  \nbaz\n``\n" ==-> "<p><code>foo bar   baz</code></p>\n"
      it "CM336" $
        "``\nfoo \n``" ==-> "<p><code>foo </code></p>\n"
      it "CM337" $
        "`foo   bar \nbaz`" ==-> "<p><code>foo   bar  baz</code></p>\n"
      it "CM338" $
        let s = "`foo\\`bar`\n"
         in s ~-> err 10 (ueib <> etok '`' <> ecsc)
      it "CM339" $
        "``foo`bar``\n" ==-> "<p><code>foo`bar</code></p>\n"
      it "CM340" $
        "` foo `` bar `" ==-> "<p><code>foo `` bar</code></p>\n"
      it "CM341" $
        let s = "*foo`*`\n"
         in s ~-> err 7 (ueib <> etok '*' <> eic)
      it "CM342" $
        let s = "[not a `link](/foo`)\n"
         in s ~-> err 20 (ueib <> etok ']' <> eic)
      it "CM343" $
        let s = "`<a href=\"`\">`\n"
         in s ~-> err 14 (ueib <> etok '`' <> ecsc)
      it "CM344" $
        "<a href=\"`\">`"
          ==-> "<p>&lt;a href=&quot;<code>&quot;&gt;</code></p>\n"
      it "CM345" $
        let s = "`<https://foo.bar.`baz>`\n"
         in s ~-> err 24 (ueib <> etok '`' <> ecsc)
      it "CM346" $
        "<https://foo.bar.`baz>`"
          ==-> "<p>&lt;https://foo.bar.<code>baz&gt;</code></p>\n"
      it "CM347" $
        let s = "```foo``\n"
         in s ~-> err 8 (ueib <> etok '`' <> ecsc)
      it "CM348" $
        let s = "`foo\n"
         in s ~-> err 4 (ueib <> etok '`' <> ecsc)
      it "CM349" $
        let s = "`foo``bar``\n"
         in s ~-> err 11 (ueib <> etok '`' <> ecsc)
    context "6.2 Emphasis and strong emphasis" $ do
      it "CM350" $
        "*foo bar*" ==-> "<p><em>foo bar</em></p>\n"
      it "CM351" $
        let s = "a * foo bar*\n"
         in s ~-> errFancy 2 (nonFlanking "*")
      it "CM352" $
        let s = "a*\"foo\"*\n"
         in s ~-> errFancy 1 (unmatchedClosing "*")
      it "CM353" $
        let s = "* a *\n"
         in s ~-> errFancy 0 (nonFlanking "*")
      -- Symbols count as punctuation, so the closing delimiter run of each
      -- of these leans left and opens emphasis instead of closing it, and
      -- is then left unclosed. CommonMark renders them literally.
      it "CM354" $ do
        "*$*alpha.\n" ~-> err 9 (ueib <> etok '*' <> eic)
        "*£*bravo.\n" ~-> err 9 (ueib <> etok '*' <> eic)
        "*€*charlie.\n" ~-> err 11 (ueib <> etok '*' <> eic)
      it "CM355" $
        "foo*bar*\n" ==-> "<p>foo<em>bar</em></p>\n"
      it "CM356" $
        "5*6*78\n" ==-> "<p>5<em>6</em>78</p>\n"
      it "CM357" $
        "_foo bar_" ==-> "<p><em>foo bar</em></p>\n"
      it "CM358" $
        let s = "_ foo bar_\n"
         in s ~-> errFancy 0 (nonFlanking "_")
      it "CM359" $
        let s = "a_\"foo\"_\n"
         in s ~-> errFancy 1 (unmatchedClosing "_")
      it "CM360" $
        let s = "foo_bar_\n"
         in s ~-> errFancy 7 (unmatchedClosing "_")
      it "CM361" $
        "5_6_78\n" ==-> "<p>5_6_78</p>\n"
      it "CM362" $
        let s = "пристаням_стремятся_\n"
         in s ~-> errFancy 19 (unmatchedClosing "_")
      it "CM363" $
        let s = "aa_\"bb\"_cc\n"
         in s ~-> errFancy 2 (unmatchedClosing "_")
      it "CM364" $
        "foo-_(bar)_\n" ==-> "<p>foo-<em>(bar)</em></p>\n"
      it "CM365" $
        let s = "_foo*\n"
         in s ~-> err 4 (utok '*' <> etok '_' <> eic)
      it "CM366" $
        let s = "*foo bar *\n"
         in s ~-> errFancy 9 (nonFlanking "*")
      it "CM367" $
        let s = "*foo bar\n*\n"
         in s ~-> err 8 (ueib <> etok '*' <> eic)
      it "CM368" $
        let s = "*(*foo)\n"
         in s ~-> err 7 (ueib <> etok '*' <> eic)
      it "CM369" $
        "*(*foo*)*"
          ==-> "<p><em>(<em>foo</em>)</em></p>\n"
      it "CM370" $
        "*foo*bar\n" ==-> "<p><em>foo</em>bar</p>\n"
      it "CM371" $
        let s = "_foo bar _\n"
         in s ~-> errFancy 9 (nonFlanking "_")
      it "CM372" $
        let s = "_(_foo)"
         in s ~-> err 7 (ueib <> etok '_' <> eic)
      it "CM373" $
        "_(_foo_)_"
          ==-> "<p><em>(<em>foo</em>)</em></p>\n"
      it "CM374" $
        let s = "_foo_bar\n"
         in s ~-> err 8 (ueib <> etok '_' <> eic)
      it "CM375" $
        let s = "_пристаням_стремятся\n"
         in s ~-> err 20 (ueib <> etok '_' <> eic)
      it "CM376" $
        "_foo_bar_baz_\n" ==-> "<p><em>foo_bar_baz</em></p>\n"
      it "CM377" $
        "_(bar)_.\n" ==-> "<p><em>(bar)</em>.</p>\n"
      it "CM378" $
        "**foo bar**\n" ==-> "<p><strong>foo bar</strong></p>\n"
      it "CM379" $
        let s = "** foo bar**\n"
         in s ~-> errFancy 0 (nonFlanking "**")
      it "CM380" $
        let s = "a**\"foo\"**\n"
         in s ~-> errFancy 1 (unmatchedClosing "**")
      it "CM381" $
        "foo**bar**\n" ==-> "<p>foo<strong>bar</strong></p>\n"
      it "CM382" $
        "__foo bar__" ==-> "<p><strong>foo bar</strong></p>\n"
      it "CM383" $
        let s = "__ foo bar__\n"
         in s ~-> errFancy 0 (nonFlanking "__")
      it "CM384" $
        let s = "__\nfoo bar__\n"
         in s ~-> errFancy 0 (nonFlanking "__")
      it "CM385" $
        let s = "a__\"foo\"__\n"
         in s ~-> errFancy 1 (unmatchedClosing "__")
      it "CM386" $
        let s = "foo__bar__\n"
         in s ~-> errFancy 8 (unmatchedClosing "__")
      it "CM387" $
        "5__6__78\n" ==-> "<p>5__6__78</p>\n"
      it "CM388" $
        let s = "пристаням__стремятся__\n"
         in s ~-> errFancy 20 (unmatchedClosing "__")
      it "CM389" $
        "__foo, __bar__, baz__"
          ==-> "<p><strong>foo, <strong>bar</strong>, baz</strong></p>\n"
      it "CM390" $
        "foo-__(bar)__" ==-> "<p>foo-<strong>(bar)</strong></p>\n"
      it "CM391" $
        let s = "**foo bar **\n"
         in s ~-> errFancy 10 (nonFlanking "**")
      it "CM392" $
        let s = "**(**foo)\n"
         in s ~-> err 9 (ueib <> etoks "**" <> eic)
      it "CM393" $
        "*(**foo**)*"
          ==-> "<p><em>(<strong>foo</strong>)</em></p>\n"
      it "CM394" $
        "**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**"
          ==-> "<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.\n<em>Asclepias physocarpa</em>)</strong></p>\n"
      it "CM395" $
        "**foo \"*bar*\" foo**"
          ==-> "<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>\n"
      it "CM396" $
        "**foo**bar\n" ==-> "<p><strong>foo</strong>bar</p>\n"
      it "CM397" $
        let s = "__foo bar __\n"
         in s ~-> errFancy 10 (nonFlanking "__")
      it "CM398" $
        let s = "__(__foo)\n"
         in s ~-> err 9 (ueib <> etoks "__" <> eic)
      it "CM399" $
        "_(__foo__)_"
          ==-> "<p><em>(<strong>foo</strong>)</em></p>\n"
      it "CM400" $
        let s = "__foo__bar\n"
         in s ~-> err 10 (ueib <> etoks "__" <> eic)
      it "CM401" $
        let s = "__пристаням__стремятся\n"
         in s ~-> err 22 (ueib <> etoks "__" <> eic)
      it "CM402" $
        "__foo__bar__baz__"
          ==-> "<p><strong>foo__bar__baz</strong></p>\n"
      it "CM403" $
        "__(bar)__."
          ==-> "<p><strong>(bar)</strong>.</p>\n"
      it "CM404" $
        "*foo [bar](/url)*"
          ==-> "<p><em>foo <a href=\"/url\">bar</a></em></p>\n"
      it "CM405" $
        "*foo\nbar*"
          ==-> "<p><em>foo\nbar</em></p>\n"
      it "CM406" $
        "_foo __bar__ baz_"
          ==-> "<p><em>foo <strong>bar</strong> baz</em></p>\n"
      it "CM407" $
        "_foo _bar_ baz_"
          ==-> "<p><em>foo <em>bar</em> baz</em></p>\n"
      it "CM408" $
        let s = "__foo_ bar_"
         in s ~-> err 5 (utoks "_ " <> etoks "__" <> eic)
      it "CM409" $
        "*foo *bar**"
          ==-> "<p><em>foo <em>bar</em></em></p>\n"
      it "CM410" $
        "*foo **bar** baz*"
          ==-> "<p><em>foo <strong>bar</strong> baz</em></p>\n"
      it "CM411" $
        "*foo**bar**baz*\n"
          ==-> "<p><em>foo<strong>bar</strong>baz</em></p>\n"
      it "CM412" $
        let s = "*foo**bar*\n"
         in s ~-> err 9 (utok '*' <> etoks "**" <> eic)
      it "CM413" $
        "***foo** bar*\n" ==-> "<p><em><strong>foo</strong> bar</em></p>\n"
      it "CM414" $
        "*foo **bar***\n" ==-> "<p><em>foo <strong>bar</strong></em></p>\n"
      it "CM415" $
        "*foo**bar***\n" ==-> "<p><em>foo<strong>bar</strong></em></p>\n"
      it "CM416" $
        "foo***bar***baz\n"
          ==-> "<p>foo<em><strong>bar</strong></em>baz</p>\n"
      it "CM417" $
        let s = "foo******bar*********baz\n"
         in s ~-> err 24 (ueib <> etoks "**" <> etok '*' <> eic)
      it "CM418" $
        "*foo **bar *baz* bim** bop*\n"
          ==-> "<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>\n"
      it "CM419" $
        "*foo [*bar*](/url)*\n"
          ==-> "<p><em>foo <a href=\"/url\"><em>bar</em></a></em></p>\n"
      it "CM420" $
        let s = "** is not an empty emphasis\n"
         in s ~-> errFancy 0 (nonFlanking "**")
      it "CM421" $
        let s = "**** is not an empty strong emphasis\n"
         in s ~-> errFancy 0 (nonFlanking "****")
      it "CM422" $
        "**foo [bar](/url)**"
          ==-> "<p><strong>foo <a href=\"/url\">bar</a></strong></p>\n"
      it "CM423" $
        "**foo\nbar**"
          ==-> "<p><strong>foo\nbar</strong></p>\n"
      it "CM424" $
        "__foo _bar_ baz__"
          ==-> "<p><strong>foo <em>bar</em> baz</strong></p>\n"
      it "CM425" $
        "__foo __bar__ baz__"
          ==-> "<p><strong>foo <strong>bar</strong> baz</strong></p>\n"
      it "CM426" $
        "____foo__ bar__"
          ==-> "<p><strong><strong>foo</strong> bar</strong></p>\n"
      it "CM427" $
        "**foo **bar****"
          ==-> "<p><strong>foo <strong>bar</strong></strong></p>\n"
      it "CM428" $
        "**foo *bar* baz**"
          ==-> "<p><strong>foo <em>bar</em> baz</strong></p>\n"
      it "CM429" $
        "**foo*bar*baz**\n"
          ==-> "<p><strong>foo<em>bar</em>baz</strong></p>\n"
      it "CM430" $
        "***foo* bar**"
          ==-> "<p><strong><em>foo</em> bar</strong></p>\n"
      it "CM431" $
        "**foo *bar***"
          ==-> "<p><strong>foo <em>bar</em></strong></p>\n"
      it "CM432" $
        "**foo *bar **baz**\nbim* bop**"
          ==-> "<p><strong>foo <em>bar <strong>baz</strong>\nbim</em> bop</strong></p>\n"
      it "CM433" $
        "**foo [*bar*](/url)**"
          ==-> "<p><strong>foo <a href=\"/url\"><em>bar</em></a></strong></p>\n"
      it "CM434" $
        let s = "__ is not an empty emphasis\n"
         in s ~-> errFancy 0 (nonFlanking "__")
      it "CM435" $
        let s = "____ is not an empty strong emphasis\n"
         in s ~-> errFancy 0 (nonFlanking "____")
      it "CM436" $
        let s = "foo ***\n"
         in s ~-> errFancy 4 (nonFlanking "***")
      it "CM437" $
        "foo *\\**" ==-> "<p>foo <em>*</em></p>\n"
      it "CM438" $
        "foo *\\_*\n" ==-> "<p>foo <em>_</em></p>\n"
      it "CM439" $
        let s = "foo *****\n"
         in s ~-> errFancy 4 (nonFlanking "*****")
      it "CM440" $
        "foo **\\***" ==-> "<p>foo <strong>*</strong></p>\n"
      it "CM441" $
        "foo **\\_**\n" ==-> "<p>foo <strong>_</strong></p>\n"
      it "CM442" $
        let s = "**foo*\n"
         in s ~-> err 5 (utok '*' <> etoks "**" <> eic)
      it "CM443" $
        let s = "*foo**\n"
         in s ~-> errFancy 5 (unmatchedClosing "*")
      it "CM444" $
        let s = "***foo**\n"
         in s ~-> err 8 (ueib <> etok '*' <> eic)
      it "CM445" $
        let s = "****foo*\n"
         in s ~-> err 7 (utok '*' <> etoks "**" <> eic)
      it "CM446" $
        let s = "**foo***\n"
         in s ~-> errFancy 7 (unmatchedClosing "*")
      it "CM447" $
        let s = "*foo****\n"
         in s ~-> errFancy 5 (unmatchedClosing "***")
      it "CM448" $
        let s = "foo ___\n"
         in s ~-> errFancy 4 (nonFlanking "___")
      it "CM449" $
        "foo _\\__" ==-> "<p>foo <em>_</em></p>\n"
      it "CM450" $
        "foo _\\*_" ==-> "<p>foo <em>*</em></p>\n"
      it "CM451" $
        let s = "foo _____\n"
         in s ~-> errFancy 4 (nonFlanking "_____")
      it "CM452" $
        "foo __\\___" ==-> "<p>foo <strong>_</strong></p>\n"
      it "CM453" $
        "foo __\\*__" ==-> "<p>foo <strong>*</strong></p>\n"
      it "CM454" $
        let s = "__foo_\n"
         in s ~-> err 5 (utok '_' <> etoks "__" <> eic)
      it "CM455" $
        let s = "_foo__\n"
         in s ~-> errFancy 5 (unmatchedClosing "_")
      it "CM456" $
        let s = "___foo__\n"
         in s ~-> err 8 (ueib <> etok '_' <> eic)
      it "CM457" $
        let s = "____foo_\n"
         in s ~-> err 7 (utok '_' <> etoks "__" <> eic)
      it "CM458" $
        let s = "__foo___\n"
         in s ~-> errFancy 7 (unmatchedClosing "_")
      it "CM459" $
        let s = "_foo____\n"
         in s ~-> errFancy 5 (unmatchedClosing "___")
      it "CM460" $
        "**foo**" ==-> "<p><strong>foo</strong></p>\n"
      it "CM461" $
        "*_foo_*" ==-> "<p><em><em>foo</em></em></p>\n"
      it "CM462" $
        "__foo__" ==-> "<p><strong>foo</strong></p>\n"
      it "CM463" $
        "_*foo*_" ==-> "<p><em><em>foo</em></em></p>\n"
      it "CM464" $
        "****foo****" ==-> "<p><strong><strong>foo</strong></strong></p>\n"
      it "CM465" $
        "____foo____" ==-> "<p><strong><strong>foo</strong></strong></p>\n"
      it "CM466" $
        "******foo******"
          ==-> "<p><strong><strong><strong>foo</strong></strong></strong></p>\n"
      it "CM467" $
        "***foo***" ==-> "<p><em><strong>foo</strong></em></p>\n"
      it "CM468" $
        "_____foo_____"
          ==-> "<p><em><strong><strong>foo</strong></strong></em></p>\n"
      it "CM469" $
        let s = "*foo _bar* baz_\n"
         in s ~-> err 9 (utok '*' <> etok '_' <> eic)
      it "CM470" $
        let s = "*foo __bar *baz bim__ bam*\n"
         in s ~-> err 19 (utok '_' <> etok '*' <> eic)
      it "CM471" $
        let s = "**foo **bar baz**\n"
         in s ~-> err 17 (ueib <> etoks "**" <> eic)
      it "CM472" $
        let s = "*foo *bar baz*\n"
         in s ~-> err 14 (ueib <> etok '*' <> eic)
      it "CM473" $
        let s = "*[bar*](/url)\n"
         in s ~-> err 5 (utok '*' <> etok ']' <> eic)
      it "CM474" $
        let s = "_foo [bar_](/url)\n"
         in s ~-> err 9 (utok '_' <> etok ']' <> eic)
      it "CM475" $
        "*<img src=\"foo\" title=\"*\"/>\n"
          ==-> "<p><em>&lt;img src=&quot;foo&quot; title=&quot;</em>&quot;/&gt;</p>\n"
      it "CM476" $
        "**<a href=\"**\">"
          ==-> "<p><strong>&lt;a href=&quot;</strong>&quot;&gt;</p>\n"
      it "CM477" $
        "__<a href=\"__\">\n"
          ==-> "<p><strong>&lt;a href=&quot;</strong>&quot;&gt;</p>\n"
      it "CM478" $
        "*a `*`*" ==-> "<p><em>a <code>*</code></em></p>\n"
      it "CM479" $
        "_a `_`_" ==-> "<p><em>a <code>_</code></em></p>\n"
      it "CM480" $
        let s = "**a<https://foo.bar/?q=**>"
         in s ~-> err 26 (ueib <> etoks "**" <> eic)
      it "CM481" $
        let s = "__a<https://foo.bar/?q=__>"
         in s ~-> err 26 (ueib <> etoks "__" <> eic)
    context "6.3 Links" $ do
      it "CM482" $
        "[link](/uri \"title\")"
          ##-> p_ (a_ [href_ "/uri", title_ "title"] "link")
      it "CM483" $
        "[link](/uri)"
          ==-> "<p><a href=\"/uri\">link</a></p>\n"
      it "CM484" $
        let s = "[](./target.md)\n"
         in s ~-> err 1 (utok ']' <> eic)
      it "CM485" $
        let s = "[link]()"
         in s
              ~-> err
                7
                (utok ')' <> etok '<' <> elabel "URI" <> ews)
      it "CM486" $
        "[link](<>)"
          ==-> "<p><a href>link</a></p>\n"
      it "CM487" $
        let s = "[]()\n"
         in s ~-> err 1 (utok ']' <> eic)
      it "CM488" $
        let s = "[link](/my uri)\n"
         in s
              ~-> err
                11
                (utok 'u' <> etok '"' <> etok '\'' <> etok '(' <> etok ')' <> ews)
      it "CM489" $
        let s = "[link](</my uri>)\n"
         in s ~-> err 11 (utok ' ' <> euric <> etok '>')
      it "CM490" $
        let s = "[link](foo\nbar)\n"
         in s
              ~-> err
                11
                (utok 'b' <> etok '"' <> etok '\'' <> etok '(' <> etok ')' <> ews)
      it "CM491" $
        let s = "[link](<foo\nbar>)\n"
         in s ~-> err 11 (utok '\n' <> euric <> etok '>')
      it "CM492" $
        "[a](<b)c>)\n" ==-> "<p><a href=\"b%29c\">a</a></p>\n"
      it "CM493" $
        let s = "[link](<foo\\>)\n"
         in s ~-> err 11 (utok '\\' <> etok '>' <> euric)
      it "CM494" $
        let s = "[a](<b)c\n[a](<b)c>\n[a](<b>c)\n"
         in s ~-> err 8 (utok '\n' <> etok '>' <> euric)
      it "CM495" $
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
      it "CM496" $
        "[link](foo(and(bar)))\n"
          ==-> "<p><a href=\"foo%28and%28bar\">link</a>))</p>\n"
      it "CM497" $
        "[link](foo(and(bar))\n"
          ==-> "<p><a href=\"foo%28and%28bar\">link</a>)</p>\n"
      it "CM498" $
        let s = "[link](foo\\(and\\(bar\\))"
         in s ~-> err 10 (utok '\\' <> euric <> euri)
      it "CM499" $
        "[link](<foo(and(bar)>)"
          ==-> "<p><a href=\"foo%28and%28bar%29\">link</a></p>\n"
      it "CM500" $
        let s = "[link](foo\\)\\:)"
         in s ~-> err 10 (utok '\\' <> euric <> euri)
      it "CM501" $
        "[link](#fragment)\n\n[link](https://example.com#fragment)\n\n[link](https://example.com?foo=3#frag)\n"
          ==-> "<p><a href=\"#fragment\">link</a></p>\n<p><a href=\"https://example.com#fragment\">link</a></p>\n<p><a href=\"https://example.com?foo=3#frag\">link</a></p>\n"
      it "CM502" $
        let s = "[link](foo\\bar)"
         in s ~-> err 10 (utok '\\' <> euric <> euri)
      it "CM503" $
        "[link](foo%20b&auml;)"
          ==-> "<p><a href=\"foo%20b%26auml%3b\">link</a></p>\n"
      it "CM504" $
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
      it "CM505" $
        "[link](/url \"title\")\n[link](/url 'title')\n[link](/url (title))"
          ##-> p_
            ( do
                a_ [href_ "/url", title_ "title"] "link"
                "\n"
                a_ [href_ "/url", title_ "title"] "link"
                "\n"
                a_ [href_ "/url", title_ "title"] "link"
            )
      it "CM506" $
        "[link](/url \"title \\\"&quot;\")\n"
          ##-> p_ (a_ [href_ "/url", title_ "title \"\""] "link")
      it "CM507" $
        let s = "[link](/url \"title\")"
         in s ~-> err 11 (utok ' ' <> euric <> euri)
      it "CM508" $
        let s = "[link](/url \"title \"and\" title\")\n"
         in s ~-> err 20 (utok 'a' <> etok ')' <> ews)
      it "CM509" $
        "[link](/url 'title \"and\" title')"
          ##-> p_ (a_ [href_ "/url", title_ "title \"and\" title"] "link")
      it "CM510" $
        "[link](   /uri\n  \"title\"  )"
          ##-> p_ (a_ [href_ "/uri", title_ "title"] "link")
      it "CM511" $
        let s = "[link] (/uri)\n"
         in s ~-> errFancy 1 (couldNotMatchRef "link" [])
      it "CM512" $
        let s = "[link [foo [bar]]](/uri)\n"
         in s ~-> err 6 (utok '[' <> etok ']' <> eic)
      it "CM513" $
        let s = "[link] bar](/uri)\n"
         in s ~-> errFancy 1 (couldNotMatchRef "link" [])
      it "CM514" $
        let s = "[link [bar](/uri)\n"
         in s ~-> err 6 (utok '[' <> etok ']' <> eic)
      it "CM515" $
        "[link \\[bar](/uri)\n"
          ==-> "<p><a href=\"/uri\">link [bar</a></p>\n"
      it "CM516" $
        "[link *foo **bar** `#`*](/uri)"
          ==-> "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"
      it "CM517" $
        "[![moon](moon.jpg)](/uri)"
          ==-> "<p><a href=\"/uri\"><img alt=\"moon\" src=\"moon.jpg\"></a></p>\n"
      it "CM518" $
        let s = "[foo [bar](/uri)](/uri)\n"
         in s ~-> err 5 (utok '[' <> etok ']' <> eic)
      it "CM519" $
        let s = "[foo *[bar [baz](/uri)](/uri)*](/uri)\n"
         in s ~-> err 6 (utok '[' <> eic)
      it "CM520" $
        let s = "![[[foo](uri1)](uri2)](uri3)"
         in s ~-> err 3 (utok '[' <> eic)
      it "CM521" $
        let s = "*[foo*](/uri)\n"
         in s ~-> err 5 (utok '*' <> etok ']' <> eic)
      it "CM522" $
        let s = "[foo *bar](baz*)\n"
         in s ~-> err 9 (utok ']' <> etok '*' <> eic)
      it "CM523" $
        let s = "*foo [bar* baz]\n"
         in s ~-> err 9 (utok '*' <> etok ']' <> eic)
      it "CM524" $
        "[foo <bar attr=\"](baz)\">"
          ==-> "<p><a href=\"baz\">foo &lt;bar attr=&quot;</a>&quot;&gt;</p>\n"
      it "CM525" $
        let s = "[foo`](/uri)`\n"
         in s ~-> err 13 (ueib <> etok ']' <> eic)
      it "CM526" $
        "[foo<https://example.com/?search=](uri)>"
          ==-> "<p><a href=\"uri\">foo&lt;https://example.com/?search=</a>&gt;</p>\n"
      it "CM527" $
        "[foo][bar]\n\n[bar]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "foo")
      it "CM528" $
        let s = "[link [foo [bar]]][ref]\n\n[ref]: /uri"
         in s ~-> err 6 (utok '[' <> etok ']' <> eic)
      it "CM529" $
        "[link \\[bar][ref]\n\n[ref]: /uri"
          ==-> "<p><a href=\"/uri\">link [bar</a></p>\n"
      it "CM530" $
        "[link *foo **bar** `#`*][ref]\n\n[ref]: /uri"
          ==-> "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"
      it "CM531" $
        "[![moon](moon.jpg)][ref]\n\n[ref]: /uri"
          ==-> "<p><a href=\"/uri\"><img alt=\"moon\" src=\"moon.jpg\"></a></p>\n"
      it "CM532" $
        let s = "[foo [bar](/uri)][ref]\n\n[ref]: /uri"
         in s ~-> err 5 (utok '[' <> etok ']' <> eic)
      it "CM533" $
        let s = "[foo *bar [baz][ref]*][ref]\n\n[ref]: /uri"
         in s ~-> err 10 (utok '[' <> etok '*' <> eic)
      it "CM534" $
        let s = "*[foo*][ref]\n\n[ref]: /uri"
         in s ~-> err 5 (utok '*' <> etok ']' <> eic)
      it "CM535" $
        let s = "[foo *bar][ref]*\n\n[ref]: /uri"
         in s ~-> err 9 (utok ']' <> etok '*' <> eic)
      it "CM536" $
        "[foo <bar attr=\"][ref]\">\n\n[ref]: /uri"
          ==-> "<p><a href=\"/uri\">foo &lt;bar attr=&quot;</a>&quot;&gt;</p>\n"
      it "CM537" $
        let s = "[foo`][ref]`\n\n[ref]: /uri"
         in s ~-> err 12 (ueib <> etok ']' <> eic)
      it "CM538" $
        "[foo<https://example.com/?search=][ref]>\n\n[ref]: /uri"
          ==-> "<p><a href=\"/uri\">foo&lt;https://example.com/?search=</a>&gt;</p>\n"
      it "CM539" $
        "[foo][BaR]\n\n[bar]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "foo")
      -- Dropped in CommonMark 0.31.2, but reference labels are still
      -- matched case-insensitively, including outside of ASCII.
      it "matches non-ASCII reference labels case-insensitively" $
        "[Толпой][Толпой] is a Russian word.\n\n[ТОЛПОЙ]: /url"
          ==-> "<p><a href=\"/url\">Толпой</a> is a Russian word.</p>\n"
      it "CM540" $
        "[\7838]\n\n[SS]: /url\n" ==-> "<p><a href=\"/url\">\7838</a></p>\n"
      it "CM541" $
        "[Foo\n  bar]: /url\n\n[Baz][Foo bar]"
          ==-> "<p><a href=\"/url\">Baz</a></p>\n"
      it "CM542" $
        let s = "[foo] [bar]\n\n[bar]: /url \"title\""
         in s ~-> errFancy 1 (couldNotMatchRef "foo" [])
      it "CM543" $
        let s = "[foo]\n[bar]\n\n[bar]: /url \"title\""
         in s ~-> errFancy 1 (couldNotMatchRef "foo" [])
      it "CM544" $
        let s = "[foo]: /url1\n\n[foo]: /url2\n\n[bar][foo]"
         in s ~-> errFancy 15 (duplicateRef "foo")
      it "CM545" $
        "[bar][foo\\!]\n\n[foo!]: /url"
          ==-> "<p><a href=\"/url\">bar</a></p>\n"
      it "CM546" $
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
      it "CM547" $
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
      it "CM548" $
        let s = "[[[foo]]]\n\n[[[foo]]]: /url"
         in s
              ~~-> [ err 1 (utok '[' <> eic),
                     err 12 (utok '[' <> eic)
                   ]
      it "CM549" $
        "[foo][ref\\[]\n\n[ref\\[]: /uri"
          ==-> "<p><a href=\"/uri\">foo</a></p>\n"
      it "CM550" $
        "[bar\\\\]: /uri\n\n[bar\\\\]"
          ==-> "<p><a href=\"/uri\">bar\\</a></p>\n"
      it "CM551" $
        let s = "[]\n\n[]: /uri"
         in s
              ~~-> [ err 1 (utok ']' <> eic),
                     err 5 (utok ']' <> eic)
                   ]
      it "CM552" $
        let s = "[\n ]\n\n[\n ]: /uri"
         in s
              ~~-> [ errFancy 1 (couldNotMatchRef "" []),
                     errFancy 7 (couldNotMatchRef "" [])
                   ]
      it "CM553" $
        "[foo][]\n\n[foo]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "foo")
      it "CM554" $
        let s = "[*foo* bar][]\n\n[*foo* bar]: /url \"title\""
         in s ~-> errFancy 1 (couldNotMatchRef "foo bar" ["*foo* bar"])
      it "CM555" $
        "[Foo][]\n\n[foo]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "Foo")
      it "CM556" $
        let s = "[foo] \n[]\n\n[foo]: /url \"title\""
         in s ~-> err 8 (utok ']' <> eic)
      it "CM557" $
        "[foo]\n\n[foo]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "foo")
      it "CM558" $
        let s = "[*foo* bar]\n\n[*foo* bar]: /url \"title\""
         in s ~-> errFancy 1 (couldNotMatchRef "foo bar" ["*foo* bar"])
      it "CM559" $
        let s = "[[*foo* bar]]\n\n[*foo* bar]: /url \"title\""
         in s ~-> err 1 (utok '[' <> eic)
      it "CM560" $
        let s = "[[bar [foo]\n\n[foo]: /url"
         in s ~-> err 1 (utok '[' <> eic)
      it "CM561" $
        "[Foo]\n\n[foo]: /url \"title\""
          ##-> p_ (a_ [href_ "/url", title_ "title"] "Foo")
      it "CM562" $
        "[foo] bar\n\n[foo]: /url"
          ==-> "<p><a href=\"/url\">foo</a> bar</p>\n"
      it "CM563" $
        let s = "\\[foo]\n\n[foo]: /url \"title\""
         in s ~-> err 5 (utok ']' <> eeib <> eic)
      it "CM564" $
        let s = "[foo*]: /url\n\n*[foo*]"
         in s ~-> err 19 (utok '*' <> etok ']' <> eic)
      it "CM565" $
        "[foo][bar]\n\n[foo]: /url1\n[bar]: /url2"
          ==-> "<p><a href=\"/url2\">foo</a></p>\n"
      it "CM566" $
        "[foo][]\n\n[foo]: /url1"
          ==-> "<p><a href=\"/url1\">foo</a></p>\n"
      it "CM567" $
        let s = "[foo]()\n\n[foo]: /url1"
         in s ~-> err 6 (utok ')' <> etok '<' <> elabel "URI" <> ews)
      it "CM568" $
        let s = "[foo](not a link)\n\n[foo]: /url1"
         in s
              ~-> err
                10
                (utok 'a' <> etok '"' <> etok '\'' <> etok '(' <> etok ')' <> ews)
      it "CM569" $
        let s = "[foo][bar][baz]\n\n[baz]: /url"
         in s ~-> errFancy 6 (couldNotMatchRef "bar" ["baz"])
      it "CM570" $
        "[foo][bar][baz]\n\n[baz]: /url1\n[bar]: /url2"
          ==-> "<p><a href=\"/url2\">foo</a><a href=\"/url1\">baz</a></p>\n"
      it "CM571" $
        let s = "[foo][bar][baz]\n\n[baz]: /url1\n[foo]: /url2"
         in s ~-> errFancy 6 (couldNotMatchRef "bar" ["baz"])
    context "6.4 Images" $ do
      it "CM572" $
        "![foo](/url \"title\")"
          ==-> "<p><img alt=\"foo\" src=\"/url\" title=\"title\"></p>\n"
      it "CM573" $
        "![foo *bar*](train.jpg \"train & tracks\")"
          ==-> "<p><img alt=\"foo bar\" src=\"train.jpg\" title=\"train &amp; tracks\"></p>\n"
      it "CM574" $
        let s = "![foo ![bar](/url)](/url2)\n"
         in s ~-> err 6 (utok '!' <> etok ']' <> eic)
      it "CM575" $
        "![foo [bar](/url)](/url2)"
          ==-> "<p><img alt=\"foo bar\" src=\"/url2\"></p>\n"
      it "CM576" $
        let s = "![foo *bar*][]\n\n[foo *bar*]: train.jpg \"train & tracks\"\n"
         in s ~-> errFancy 2 (couldNotMatchRef "foo bar" ["foo *bar*"])
      it "CM577" $
        "![foo *bar*][foobar]\n\n[FOOBAR]: train.jpg \"train & tracks\""
          ==-> "<p><img alt=\"foo bar\" src=\"train.jpg\" title=\"train &amp; tracks\"></p>\n"
      it "CM578" $
        "![foo](train.jpg)"
          ==-> "<p><img alt=\"foo\" src=\"train.jpg\"></p>\n"
      it "CM579" $
        "My ![foo bar](/path/to/train.jpg  \"title\"   )"
          ==-> "<p>My <img alt=\"foo bar\" src=\"/path/to/train.jpg\" title=\"title\"></p>\n"
      it "CM580" $
        "![foo](<url>)"
          ==-> "<p><img alt=\"foo\" src=\"url\"></p>\n"
      it "CM581" $
        "![](/url)" ==-> "<p><img alt src=\"/url\"></p>\n"
      it "CM582" $
        "![foo][bar]\n\n[bar]: /url"
          ==-> "<p><img alt=\"foo\" src=\"/url\"></p>\n"
      it "CM583" $
        "![foo][bar]\n\n[BAR]: /url"
          ==-> "<p><img alt=\"foo\" src=\"/url\"></p>\n"
      it "CM584" $
        "![foo][]\n\n[foo]: /url \"title\""
          ==-> "<p><img alt=\"foo\" src=\"/url\" title=\"title\"></p>\n"
      it "CM585" $
        "![foo bar][]\n\n[foo bar]: /url \"title\""
          ==-> "<p><img alt=\"foo bar\" src=\"/url\" title=\"title\"></p>\n"
      it "CM586" $
        "![Foo][]\n\n[foo]: /url \"title\""
          ==-> "<p><img alt=\"Foo\" src=\"/url\" title=\"title\"></p>\n"
      it "CM587" $
        let s = "![foo] \n[]\n\n[foo]: /url \"title\""
         in s ~-> err 9 (utok ']' <> eic)
      it "CM588" $
        "![foo]\n\n[foo]: /url \"title\""
          ==-> "<p><img alt=\"foo\" src=\"/url\" title=\"title\"></p>\n"
      it "CM589" $
        "![*foo* bar]\n\n[foo bar]: /url \"title\"\n"
          ==-> "<p><img alt=\"foo bar\" src=\"/url\" title=\"title\"></p>\n"
      it "CM590" $
        let s = "![[foo]]\n\n[[foo]]: /url \"title\""
         in s
              ~~-> [ errFancy 3 (couldNotMatchRef "foo" []),
                     err 11 (utok '[' <> eic)
                   ]
      it "CM591" $
        "![Foo]\n\n[foo]: /url \"title\""
          ==-> "<p><img alt=\"Foo\" src=\"/url\" title=\"title\"></p>\n"
      it "CM592" $
        "!\\[foo\\]\n\n[foo]: /url \"title\""
          ==-> "<p>![foo]</p>\n"
      it "CM593" $
        "\\![foo]\n\n[foo]: /url \"title\""
          ##-> p_
            ( do
                "!"
                a_ [href_ "/url", title_ "title"] "foo"
            )
    context "6.5 Autolinks" $ do
      it "CM594" $
        "<http://foo.bar.baz>"
          ==-> "<p><a href=\"http://foo.bar.baz\">http://foo.bar.baz</a></p>\n"
      it "CM595" $
        "<https://foo.bar.baz/test?q=hello&id=22&boolean>"
          ==-> "<p><a href=\"https://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean\">https://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>\n"
      it "CM596" $
        "<irc://foo.bar:2233/baz>"
          ==-> "<p><a href=\"irc://foo.bar:2233/baz\">irc://foo.bar:2233/baz</a></p>\n"
      it "CM597" $
        "<MAILTO:FOO@BAR.BAZ>"
          ==-> "<p><a href=\"mailto:FOO@BAR.BAZ\">FOO@BAR.BAZ</a></p>\n"
      it "CM598" $
        "<a+b+c:d>"
          ==-> "<p><a href=\"a+b+c:d\">a+b+c:d</a></p>\n"
      it "CM599" $
        "<made-up-scheme://foo,bar>"
          ==-> "<p><a href=\"made-up-scheme://foo/%2cbar\">made-up-scheme://foo/%2cbar</a></p>\n"
      it "CM600" $
        "<https://../>"
          ==-> "<p><a href=\"https://..\">https://..</a></p>\n"
      it "CM601" $
        "<localhost:5001/foo>"
          ==-> "<p><a href=\"localhost:5001/foo\">localhost:5001/foo</a></p>\n"
      it "CM602" $
        "<https://foo.bar/baz bim>\n"
          ==-> "<p>&lt;https://foo.bar/baz bim&gt;</p>\n"
      it "CM603" $
        "<https://example.com/\\[\\>"
          ==-> "<p>&lt;https://example.com/[&gt;</p>\n"
      it "CM604" $
        "<foo@bar.example.com>"
          ==-> "<p><a href=\"mailto:foo@bar.example.com\">foo@bar.example.com</a></p>\n"
      it "CM605" $
        "<foo+special@Bar.baz-bar0.com>"
          ==-> "<p><a href=\"mailto:foo%2bspecial@Bar.baz-bar0.com\">foo+special@Bar.baz-bar0.com</a></p>\n"
      it "CM606" $
        "<foo\\+@bar.example.com>"
          ==-> "<p>&lt;foo+@bar.example.com&gt;</p>\n"
      it "CM607" $
        "<>"
          ==-> "<p>&lt;&gt;</p>\n"
      it "CM608" $
        "< https://foo.bar >"
          ==-> "<p>&lt; https://foo.bar &gt;</p>\n"
      it "CM609" $
        "<m:abc>"
          ==-> "<p><a href=\"m:abc\">m:abc</a></p>\n"
      it "CM610" $
        "<foo.bar.baz>"
          ==-> "<p><a href=\"foo.bar.baz\">foo.bar.baz</a></p>\n"
      it "CM611" $
        "https://example.com"
          ==-> "<p>https://example.com</p>\n"
      it "CM612" $
        "foo@bar.example.com"
          ==-> "<p>foo@bar.example.com</p>\n"
    context "6.6 Raw HTML" $
      -- NOTE We do not support raw HTML, see the readme.
      return ()
    context "6.7 Hard line breaks" $ do
      -- NOTE We currently do not support hard line breaks represented in
      -- markup as two spaces before newline.
      it "CM633" $
        "foo  \nbaz"
          ==-> "<p>foo\nbaz</p>\n"
      it "CM634" $
        "foo\\\nbaz\n"
          ==-> "<p>foo<br>\nbaz</p>\n"
      it "CM635" $
        "foo       \nbaz"
          ==-> "<p>foo\nbaz</p>\n"
      it "CM636" $
        "foo  \n     bar"
          ==-> "<p>foo\nbar</p>\n"
      it "CM637" $
        "foo\\\n     bar"
          ==-> "<p>foo<br>\nbar</p>\n"
      it "CM638" $
        "*foo  \nbar*"
          ==-> "<p><em>foo\nbar</em></p>\n"
      it "CM639" $
        "*foo\\\nbar*"
          ==-> "<p><em>foo<br>\nbar</em></p>\n"
      it "CM640" $
        "`code  \nspan`"
          ==-> "<p><code>code   span</code></p>\n"
      it "CM641" $
        "`code\\\nspan`"
          ==-> "<p><code>code\\ span</code></p>\n"
      it "CM642" $
        "<a href=\"foo  \nbar\">"
          ==-> "<p>&lt;a href=&quot;foo\nbar&quot;&gt;</p>\n"
      it "CM643" $
        "<a href=\"foo\\\nbar\">"
          ==-> "<p>&lt;a href=&quot;foo<br>\nbar&quot;&gt;</p>\n"
      it "CM644" $
        "foo\\"
          ==-> "<p>foo\\</p>\n"
      it "CM645" $
        "foo  "
          ==-> "<p>foo</p>\n"
      it "CM646" $
        "### foo\\"
          ==-> "<h3 id=\"foo\">foo\\</h3>\n"
      it "CM647" $
        "### foo  "
          ==-> "<h3 id=\"foo\">foo</h3>\n"
    context "6.8 Soft line breaks" $ do
      it "CM648" $
        "foo\nbaz"
          ==-> "<p>foo\nbaz</p>\n"
      it "CM649" $
        "foo \n baz"
          ==-> "<p>foo\nbaz</p>\n"
    context "6.9 Textual content" $ do
      it "CM650" $
        "hello $.;'there"
          ==-> "<p>hello $.;&#39;there</p>\n"
      it "CM651" $
        "Foo χρῆν"
          ==-> "<p>Foo χρῆν</p>\n"
      it "CM652" $
        "Multiple     spaces"
          ==-> "<p>Multiple     spaces</p>\n"
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
      it "nests a subscript the way strong emphasis nests emphasis" $ do
        "~~foo~bar~baz~~"
          ==-> "<p><del>foo<sub>bar</sub>baz</del></p>\n"
        "**foo*bar*baz**"
          ==-> "<p><strong>foo<em>bar</em>baz</strong></p>\n"
      it "does not lend a subscript one of its closing tildes" $ do
        "~~foo~bar~~" ~-> err 10 (utok '~' <> etoks "~~" <> eic)
        "**foo*bar**" ~-> err 10 (utok '*' <> etoks "**" <> eic)
    context "subscript" $ do
      it "works in simplest form" $
        "It's ~bad~ news."
          ==-> "<p>It&#39;s <sub>bad</sub> news.</p>\n"
      it "combines with emphasis" $
        "**It's ~bad~** news."
          ==-> "<p><strong>It&#39;s <sub>bad</sub></strong> news.</p>\n"
      it "works inside a word" $
        "H~2~O is not O~2~." ==-> "<p>H<sub>2</sub>O is not O<sub>2</sub>.</p>\n"
    context "superscript" $ do
      it "works in simplest form" $
        "It's ^bad^ news."
          ==-> "<p>It&#39;s <sup>bad</sup> news.</p>\n"
      it "combines with emphasis" $
        "**It's ^bad^** news."
          ==-> "<p><strong>It&#39;s <sup>bad</sup></strong> news.</p>\n"
      it "works inside a word" $
        "x^2^ + y^2^ = z^2^"
          ==-> "<p>x<sup>2</sup> + y<sup>2</sup> = z<sup>2</sup></p>\n"
    context "delimiter runs inside words" $ do
      it "an underscore inside a word is literal" $
        "snake_case and __dunder__ and to_string()"
          ==-> "<p>snake_case and <strong>dunder</strong> and to_string()</p>\n"
      it "an underscore inside a word does not close a frame" $
        "*a_b_c*" ==-> "<p><em>a_b_c</em></p>\n"
      it "an asterisk inside a word opens and closes a frame" $
        "un*frigging*believable"
          ==-> "<p>un<em>frigging</em>believable</p>\n"
      it "an ambiguous run closes the frame it is inside of" $
        "**foo**bar" ==-> "<p><strong>foo</strong>bar</p>\n"
      it "an ambiguous run that closes nothing opens a frame" $
        "*foo**bar**baz*"
          ==-> "<p><em>foo<strong>bar</strong>baz</em></p>\n"
      it "a run that closes nothing at all is an error" $
        let s = "foo*bar\n"
         in s ~-> err 7 (ueib <> etok '*' <> eic)
      it "a closing run without an opening one is an error" $
        let s = "foo and bar*\n"
         in s ~-> errFancy 11 (unmatchedClosing "*")
      it "a composite, complex example" $
        "***Something ~~~is not~~ going~ ^so well^** today*."
          ==-> "<p><em><strong>Something <sub><del>is not</del> going</sub> <sup>so well</sup></strong> today</em>.</p>\n"
    context "code spans (special cases)" $ do
      it "preserves white space verbatim" $ do
        "`col1  col2`" ==-> "<p><code>col1  col2</code></p>\n"
        "`a\tb`" ==-> "<p><code>a\tb</code></p>\n"
        "`  `" ==-> "<p><code>  </code></p>\n"
      it "strips one space from each end only when both are there" $ do
        "` both `" ==-> "<p><code>both</code></p>\n"
        "` a`" ==-> "<p><code> a</code></p>\n"
        "`a `" ==-> "<p><code>a </code></p>\n"
      -- The indentation of a continuation line belongs to the block that
      -- contains the paragraph, not to the code span, so it goes away with
      -- the line ending that precedes it.
      it "drops the indentation of a continuation line" $ do
        "`foo\nbar`" ==-> "<p><code>foo bar</code></p>\n"
        "`foo\n   bar`" ==-> "<p><code>foo bar</code></p>\n"
      it "drops the block quote markers of a continuation line" $ do
        "> `foo\n> bar`"
          ==-> "<blockquote>\n<p><code>foo bar</code></p>\n</blockquote>\n"
        ">   `foo\n>      bar`"
          ==-> "<blockquote>\n<p><code>foo bar</code></p>\n</blockquote>\n"
      it "keeps white space inside a block quote" $
        "> `a  b`"
          ==-> "<blockquote>\n<p><code>a  b</code></p>\n</blockquote>\n"
    context "collapsed reference links (special cases)"
      $ it "offsets after such links are still correct"
      $ "[foo][] *foo\n\n[foo]: https://example.org"
        ~-> err
          12
          (ueib <> etok '*' <> eic)
    context "title parse errors"
      $ it "parse error is OK in reference definitions"
      $ let s = "[something]: something something"
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
                     errFancy 32 (unmatchedClosing "_")
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
      it "block quotes have higher precedence than tables" $
        "> foo | bar\n> -----|----\n> baz | quux"
          ==-> "<blockquote>\n<table>\n<thead>\n<tr><th>foo</th><th>bar</th></tr>\n</thead>\n<tbody>\n<tr><td>baz</td><td>quux</td></tr>\n</tbody>\n</table>\n</blockquote>\n"
      it "if table is indented inside unordered list, it's put there" $
        "+ foo | bar\n  ----|----\n"
          ==-> "<ul>\n<li>\n<table>\n<thead>\n<tr><th>foo</th><th>bar</th></tr>\n</thead>\n<tbody>\n</tbody>\n</table>\n</li>\n</ul>\n"
      it "if table is indented inside ordered list, it's put there" $
        "1. foo | bar\n   ----|----\n"
          ==-> "<ol>\n<li>\n<table>\n<thead>\n<tr><th>foo</th><th>bar</th></tr>\n</thead>\n<tbody>\n</tbody>\n</table>\n</li>\n</ol>\n"
      it "renders a comprehensive table correctly" $
        withFiles "data/table.md" "data/table.html"
    context "parse errors at block level" $ do
      it "reports a heading that has no content" $
        "#" ~-> err 1 (ueib <> etok '#' <> ews)
      it "reports a heading with too many hash signs" $
        "####### foo" ~-> err 6 (utok '#' <> ews)
      it "a YAML block does not shift the offsets that follow it" $ do
        "---\nfoo: bar\n---\n\n*baz"
          ~-> err 22 (ueib <> etok '*' <> eic)
        "---\nfoo: bar\n---\n\n> *baz"
          ~-> err 24 (ueib <> etok '*' <> eic)
    context "parse errors in block quotes" $ do
      -- NOTE The block quote markers are replaced by spaces in the text that
      -- is handed over to the inline-level parser, so offsets inside a block
      -- quote must come out exactly as they would without it.
      it "reports an error in a one-line block quote" $ do
        "> *foo" ~-> err 6 (ueib <> etok '*' <> eic)
        "  > *foo" ~-> err 8 (ueib <> etok '*' <> eic)
      it "block quote markers do not shift offsets" $ do
        "> foo\n> *bar" ~-> err 12 (ueib <> etok '*' <> eic)
        "> > foo\n> > *bar" ~-> err 16 (ueib <> etok '*' <> eic)
        ">foo\n>   *bar" ~-> err 13 (ueib <> etok '*' <> eic)
        ">\t*foo" ~-> err 6 (ueib <> etok '*' <> eic)
      it "offsets are correct on lazy continuation lines" $ do
        "> foo\n*bar" ~-> err 10 (ueib <> etok '*' <> eic)
        "> *foo\n  bar" ~-> err 12 (ueib <> etok '*' <> eic)
        ">>> foo\n> *bar" ~-> err 14 (ueib <> etok '*' <> eic)
        "> 1. > *foo\n> continued *bar"
          ~-> err 28 (ueib <> etok '*' <> eic)
      it "offsets are correct in inlines that span several lines" $ do
        "> `foo\n> bar" ~-> err 12 (ueib <> etok '`' <> ecsc)
        "> foo\n*bar `baz" ~-> err 15 (ueib <> etok '`' <> ecsc)
      it "offsets after a block quote are not affected by it" $
        "> quote\n\n*after" ~-> err 15 (ueib <> etok '*' <> eic)
      it "reports an error in a heading in a block quote" $ do
        "> # *foo" ~-> err 8 (ueib <> etok '*' <> eic)
        ">#Header" ~-> err 2 (utok 'H' <> etok '#' <> ews)
      it "reports an error in a table cell in a block quote" $
        "> foo | bar\n> -----|----\n> *baz | quux"
          ~-> err 31 (ueib <> etok '*' <> eic)
      it "reports an error in a title in a block quote" $
        "> ![img](/url \"title\n"
          ~-> err
            20
            ( ueib
                <> etok '\"'
                <> etok '&'
                <> etoks "&#"
                <> elabel "escaped character"
                <> elabel "unescaped character"
            )
      it "reports reference definition errors in a block quote" $ do
        "> [foo]\n\n[bar]: /url"
          ~-> errFancy 3 (couldNotMatchRef "foo" [])
        "> [foo]: /url\n> [foo]: /bar"
          ~-> errFancy 17 (duplicateRef "foo")
      it "reports entity errors in a block quote" $ do
        "> &nosuchentity;" ~-> errFancy 2 (unknownEntity "nosuchentity")
        "> &#0;" ~-> errFancy 2 (invalidNumChar 0)
      it "reports every error in a block quote" $ do
        let e = ueib <> etok '*' <> eic
        "> *foo\n>\n> *bar" ~~-> [err 6 e, err 15 e]
        "> *foo\n> ***\n> *bar" ~~-> [err 6 e, err 19 e]
        "> *foo\n\n> *bar" ~~-> [err 6 e, err 14 e]
      it "reports errors in lists inside a block quote" $ do
        let e = ueib <> etok '*' <> eic
        "> - *foo\n> - *bar" ~~-> [err 8 e, err 17 e]
        "> 1. *foo\n> 3. *bar"
          ~~-> [ err 9 e,
                 errFancy 12 (indexNonCons 3 2),
                 err 19 e
               ]
      it "reports errors in a block quote inside a list" $
        "- *foo\n\n  > *bar"
          ~~-> [ err 6 (ueib <> etok '*' <> eic),
                 err 16 (ueib <> etok '*' <> eic)
               ]
      it "reports errors around a block quote in correct order" $ do
        let e = ueib <> etok '*' <> eic
        "*foo\n\n> *bar\n\n*baz" ~~-> [err 4 e, err 12 e, err 18 e]
        -- A block quote may interrupt a paragraph and be interrupted by a
        -- heading, without either losing its parse error.
        "*foo\n> *bar" ~~-> [err 4 e, err 11 e]
        "> *foo\n# *bar" ~~-> [err 6 e, err 13 e]
      -- NOTE Unlike in CommonMark, the end of a block quote does not close
      -- a code fence that was opened inside of it, see CM128 and CM237.
      describe "code fences that the end of a block quote leaves unclosed" $ do
        it "reports the line that lacks the block quote marker" $ do
          "> ```\n> foo\n" ~-> err 12 (ebqm <> eccf <> ecbc)
          "> foo\n\n> ```\n> bar\n\nbaz" ~-> err 19 (ebqm <> eccf <> ecbc)
        it "reports the marker of the innermost block quote" $
          "> > ```\n> > foo\n> ```" ~-> err 18 (ebqm <> eccf <> ecbc)
        it "works for a block quote inside a list" $
          "- > ```\n  > foo\n\nbar" ~-> err 16 (ebqm <> eccf <> ecbc)
        it "names the missing fence when the last line has no line ending" $ do
          "```\nfoo" ~-> err 7 (ueof <> eccf <> ecbc)
          "> ```\n> foo" ~-> err 11 (ueof <> eccf <> ecbc)
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
    context "given a complete, comprehensive document"
      $ it "outputs expected the HTML fragment"
      $ withFiles "data/comprehensive.md" "data/comprehensive.html"
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

-- | Expecting a block quote marker.
ebqm :: ET s
ebqm = elabel "block quote marker"

-- | Expecting a closing code fence.
eccf :: ET s
eccf = elabel "closing code fence"

-- | Expecting code block content.
ecbc :: ET s
ecbc = elabel "code block content"

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

unmatchedClosing :: Text -> EF MMarkErr
unmatchedClosing =
  fancy . ErrorCustom . UnmatchedClosingDelimiterRun . NE.fromList . T.unpack

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
