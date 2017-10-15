{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMarkSpec (spec) where

import Data.Char
import Data.Monoid
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec
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
      xit "CM16" $
        "--\n**\n__" ==-> "<p>--\n**\n__</p>\n"
      it "CM17" $
        " ***\n  ***\n   ***" ==-> "<hr>\n<hr>\n<hr>\n"
      it "CM18" $
        "    ***" ==-> "<pre><code>***\n</code></pre>\n"
      xit "CM19" $
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
      xit "CM25" $
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
    context "4.4 Indented code blocks" $ do
      it "CM76" $
        "    a simple\n      indented code block" ==->
          "<pre><code>a simple\n  indented code block\n</code></pre>\n"
      xit "CM77" $ -- FIXME pending lists
        "  - foo\n\n    bar" ==->
          "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      xit "CM78" $ -- FIXME pending lists
        "1.  foo\n\n    - bar" ==->
           "<ol>\n<li>\n<p>foo</p>\n<ul>\n<li>bar</li>\n</ul>\n</li>\n</ol>\n"
      it "CM79" $
        "    <a/>\n    *hi*\n\n    - one" ==->
          "<pre><code>&lt;a/&gt;\n*hi*\n\n- one\n</code></pre>\n"
      it "CM80" $
        "    chunk1\n\n    chunk2\n  \n \n \n    chunk3" ==->
          "<pre><code>chunk1\n\nchunk2\n\n\n\nchunk3\n</code></pre>\n"
      it "CM81" $
        "    chunk1\n      \n      chunk2" ==->
          "<pre><code>chunk1\n  \n  chunk2\n</code></pre>\n"
      it "CM82" $
        "Foo\n    bar\n" ==->
          "<p>Foo\nbar</p>\n"
      it "CM83" $
        "    foo\nbar" ==->
          "<pre><code>foo\n</code></pre>\n<p>bar</p>\n"
      xit "CM84" $ -- FIXME pending setext headings
        "# Heading\n    foo\nHeading\n------\n    foo\n----\n" ==->
          "<h1>Heading</h1>\n<pre><code>foo\n</code></pre>\n<h2>Heading</h2>\n<pre><code>foo\n</code></pre>\n<hr />\n"
      it "CM85" $
        "        foo\n    bar" ==->
          "<pre><code>    foo\nbar\n</code></pre>\n"
      it "CM86" $
        "\n    \n    foo\n    \n" ==->
          "<pre><code>foo\n</code></pre>\n"
      it "CM87" $
        "    foo  " ==->
          "<pre><code>foo  \n</code></pre>\n"
    context "4.5 Fenced code blocks" $ do
      it "CM88" $
        "```\n<\n >\n```" ==->
          "<pre><code>&lt;\n &gt;\n</code></pre>\n"
      it "CM89" $
        "~~~\n<\n >\n~~~" ==->
          "<pre><code>&lt;\n &gt;\n</code></pre>\n"
      it "CM90" $
        "```\naaa\n~~~\n```" ==->
          "<pre><code>aaa\n~~~\n</code></pre>\n"
      it "CM91" $
        "~~~\naaa\n```\n~~~" ==->
          "<pre><code>aaa\n```\n</code></pre>\n"
      it "CM92" $
        "````\naaa\n```\n``````" ==->
          "<pre><code>aaa\n```\n</code></pre>\n"
      it "CM93" $
        "~~~~\naaa\n~~~\n~~~~" ==->
          "<pre><code>aaa\n~~~\n</code></pre>\n"
      it "CM94" $ do
        let s = "```"
        s ~->
          [ err (posN 3 s)
              (ueof <> etok '`' <> elabel "info string" <> elabel "newline")
          ]
      it "CM95" $ do
        let s  = "`````\n\n```\naaa"
            s' = s <> "\n"
        s ~->
          [ err (posN 14 s) (ueof <> elabel "newline")
          ]
        s' ~->
          [ err (posN 15 s') (ueof <> elabel "closing code fence"
                               <> elabel "code block content")
          ]
      xit "CM96" $ -- FIXME pending blockquotes
        "> ```\n> aaa\n\nbbb" ==->
          "<blockquote>\n<pre><code>aaa\n</code></pre>\n</blockquote>\n<p>bbb</p>\n"
      it "CM97" $
        "```\n\n  \n```" ==->
          "<pre><code>\n  \n</code></pre>\n"
      it "CM98" $
        "```\n```" ==->
          "<pre><code></code></pre>\n"
      it "CM99" $
        " ```\n aaa\naaa\n```" ==->
          "<pre><code>aaa\naaa\n</code></pre>\n"
      it "CM100" $
        "  ```\naaa\n  aaa\naaa\n  ```" ==->
          "<pre><code>aaa\naaa\naaa\n</code></pre>\n"
      it "CM101" $
        "   ```\n   aaa\n    aaa\n  aaa\n   ```" ==->
          "<pre><code>aaa\n aaa\naaa\n</code></pre>\n"
      it "CM102" $
        "    ```\n    aaa\n    ```" ==->
          "<pre><code>```\naaa\n```\n</code></pre>\n"
      it "CM103" $
        "```\naaa\n  ```" ==->
          "<pre><code>aaa\n</code></pre>\n"
      it "CM104" $
        "   ```\naaa\n  ```" ==->
          "<pre><code>aaa\n</code></pre>\n"
      it "CM105" $ do
        let s  = "```\naaa\n    ```"
            s' = s <> "\n"
        s ~->
          [ err (posN 15 s) (ueof <> elabel "newline")
          ]
        s' ~->
          [ err (posN 16 s') (ueof <> elabel "closing code fence"
                               <> elabel "code block content")
          ]
      it "CM106" $
        "``` ```\naaa" ==->
          "<p><code></code>\naaa</p>\n"
      it "CM107" $ do
        let s  = "~~~~~~\naaa\n~~~ ~~"
            s' = s <> "\n"
        s ~->
          [ err (posN 17 s) (ueof <> elabel "newline")
          ]
        s' ~->
          [ err (posN 18 s') (ueof <> elabel "closing code fence"
                               <> elabel "code block content")
          ]
      it "CM108" $
        "foo\n```\nbar\n```\nbaz" ==->
          "<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n"
      xit "CM109" $ -- FIXME pending setext headings
        "foo\n---\n~~~\nbar\n~~~\n# baz" ==->
          "<h2>foo</h2>\n<pre><code>bar\n</code></pre>\n<h1>baz</h1>\n"
      it "CM110" $
        "```ruby\ndef foo(x)\n  return 3\nend\n```" ==->
          "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
      it "CM111" $
        "~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~" ==->
          "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
      it "CM112" $
        "````;\n````" ==->
          "<pre><code class=\"language-;\"></code></pre>\n"
      it "CM113" $
        "``` aa ```\nfoo" ==->
          "<p><code>aa</code>\nfoo</p>\n"
      it "CM114" $
        "```\n``` aaa\n```" ==->
          "<pre><code>``` aaa\n</code></pre>\n"
    context "4.8 Paragraphs" $ do
      it "CM180" $
        "aaa\n\nbbb" ==->
          "<p>aaa</p>\n<p>bbb</p>\n"
      it "CM181" $
        "aaa\nbbb\n\nccc\nddd" ==->
          "<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n"
      it "CM182" $
        "aaa\n\n\nbbb" ==->
          "<p>aaa</p>\n<p>bbb</p>\n"
      it "CM183" $
        "  aaa\n bbb" ==->
          "<p>aaa\nbbb</p>\n"
      it "CM184" $
        "aaa\n             bbb\n                                       ccc" ==->
          "<p>aaa\nbbb\nccc</p>\n"
      it "CM185" $
        "   aaa\nbbb" ==-> "<p>aaa\nbbb</p>\n"
      it "CM186" $
        "    aaa\nbbb" ==->
          "<pre><code>aaa\n</code></pre>\n<p>bbb</p>\n"
      it "CM187" $
        "aaa     \nbbb     " ==->
          "<p>aaa\nbbb</p>\n"
    context "4.9 Blank lines" $
      it "CM188" $
        "  \n\naaa\n  \n\n# aaa\n\n  " ==->
          "<p>aaa</p>\n<h1>aaa</h1>\n"
    context "6 Inlines" $
      it "CM286" $ do
        let s  = "`hi`lo`"
            s' = s <> "\n"
            pe = ueib <> etok '`' <> elabel "code span content"
        s  ~-> [ err (posN 7 s)  pe ]
        s' ~-> [ err (posN 7 s') pe ]
    context "6.1 Blackslash escapes" $ do
      it "CM287" $
        "\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~\n"
          ==-> "<p>!&quot;#$%&amp;&#39;()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>\n"
      it "CM288" $
        "\\\t\\A\\a\\ \\3\\φ\\«" ==->
          "<p>\\\t\\A\\a\\ \\3\\φ\\«</p>\n"
      it "CM289" $
        "\\*not emphasized\\*\n\\<br/> not a tag\n\\[not a link\\](/foo)\n\\`not code\\`\n1\\. not a list\n\\* not a list\n\\# not a heading\n\\[foo\\]: /url \"not a reference\"\n" ==->
        "<p>*not emphasized*\n&lt;br/&gt; not a tag\n[not a link](/foo)\n`not code`\n1. not a list\n* not a list\n# not a heading\n[foo]: /url &quot;not a reference&quot;</p>\n"
      it "CM290" $
        "\\\\*emphasis*" ==->
          "<p>\\<em>emphasis</em></p>\n"
      xit "CM291" $
        "foo\\\nbar" ==->
          "<p>foo<br>\nbar</p>\n"
      it "CM292" $
        "`` \\[\\` ``" ==->
          "<p><code>\\[\\`</code></p>\n"
      it "CM293" $
        "    \\[\\]" ==->
          "<pre><code>\\[\\]\n</code></pre>\n"
      it "CM294" $
        "~~~\n\\[\\]\n~~~" ==->
          "<pre><code>\\[\\]\n</code></pre>\n"
      xit "CM295" $ -- FIXME pending autolinks
        "<http://example.com?find=\\*>" ==->
          "<p><a href=\"http://example.com?find=%5C*\">http://example.com?find=\\*</a></p>\n"
      xit "CM296" $ -- FIXME pending HTML inlines
        "<a href=\"/bar\\/)\">" ==->
          "<a href=\"/bar\\/)\">\n"
      it "CM297" $
        "[foo](/bar\\* \"ti\\*tle\")" ==->
          "<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>\n"
      xit "CM298" $ -- FIXME pending reference links
        "[foo]\n\n[foo]: /bar\\* \"ti\\*tle\"" ==->
          "<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>\n"
      it "CM299" $
        "``` foo\\+bar\nfoo\n```" ==->
          "<pre><code class=\"language-foo+bar\">foo\n</code></pre>\n"
    context "6.2 Entity and numeric character references" $
      xit "CM300" $ -- FIXME pending entity references
        "&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;"
          ==-> "<p>  &amp; © Æ Ď\n¾ ℋ ⅆ\n∲ ≧̸</p>\n"
    context "6.3 Code spans" $ do
      it "CM312" $
        "`foo`" ==-> "<p><code>foo</code></p>\n"
      it "CM313" $
        "`` foo ` bar  ``" ==->
          "<p><code>foo ` bar</code></p>\n"
      it "CM314" $
        "` `` `" ==-> "<p><code>``</code></p>\n"
      it "CM315" $
        "``\nfoo\n``" ==-> "<p><code>foo</code></p>\n"
      it "CM316" $
        "`foo   bar\n  baz`" ==-> "<p><code>foo bar baz</code></p>\n"
      it "CM317" $
        "`a  b`" ==-> "<p><code>a  b</code></p>\n"
      it "CM318" $
        "`foo `` bar`" ==-> "<p><code>foo `` bar</code></p>\n"
      it "CM319" $
        "`foo\\`bar`" ==-> "<p><code>foo\\</code>bar`</p>\n"
      it "CM320" $
        "*foo`*`" ==-> "<p>*foo<code>*</code></p>\n"
      it "CM321" $
        "[not a `link](/foo`)" ==->
          "<p>[not a <code>link](/foo</code>)</p>\n"
      it "CM322" $
        "`<a href=\"`\">`" ==->
          "<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>\n"
      it "CM323" $
        "<a href=\"`\">`" ==->
          "<p><a href=\"`\">`</p>\n"
      it "CM324" $
        "`<http://foo.bar.`baz>`" ==->
          "<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>\n"
      it "CM325" $
        "<http://foo.bar.`baz>`" ==->
          "<p><a href=\"http://foo.bar.%60baz\">http://foo.bar.`baz</a>`</p>\n"
      it "CM326" $ do
        let s  = "```foo``"
            s' = s <> "\n"
        s ~->
          [ err (posN 8 s) (ueib <> etok '`' <> elabel "code span content")
          ]
        s' ~->
          [ err (posN 8 s') (ueib <> etok '`' <> elabel "code span content")
          ]
      it "CM327" $ do
        let s  = "`foo"
            s' = s <> "\n"
        s ~->
          [ err (posN 4 s) (ueib <> etok '`' <> elabel "code span content")
          ]
        s' ~->
          [ err (posN 4 s') (ueib <> etok '`' <> elabel "code span content")
          ]
      it "CM328" $ do
        let s  = "`foo``bar``"
            s' = s <> "\n"
        s ~->
          [ err (posN 11 s) (ueib <> etok '`' <> elabel "code span content")
          ]
        s' ~->
          [ err (posN 11 s') (ueib <> etok '`' <> elabel "code span content")
          ]
    context "6.4 Emphasis and strong emphasis" $ do
      it "CM329" $
        "*foo bar*" ==-> "<p><em>foo bar</em></p>\n"
      it "CM330" $ do
        let s = "a * foo bar*"
        s ~-> [ err (posN 2 s) (utok '*') ]
      it "CM331" $ do
        let s = "a*\"foo\"*"
        s ~-> [ err (posN 1 s) (utok '*') ]
    context "6.5 Links" $ do
      it "CM457" $
        "[link](/uri \"title\")" ==->
          "<p><a href=\"/uri\" title=\"title\">link</a></p>\n"
      it "CM458" $
        "[link](/uri)" ==->
          "<p><a href=\"/uri\">link</a></p>\n"
      it "CM459" $
        "[link]()" ==->
          "<p><a href>link</a></p>\n"
      it "CM460" $
        "[link](<>)" ==->
          "<p><a href>link</a></p>\n"
      it "CM461" $
        "[link](/my uri)" ==-> "<p>[link](/my uri)</p>\n"
      it "CM462" $ do
        let s  = "[link](</my uri>)"
            s' = s <> "\n"
            pe = utok ' ' <> etok '>' <> elabel "escaped link character"
              <> elabel "unescaped link character"
        s  ~-> [ err (posN 11 s)  pe ]
        s' ~-> [ err (posN 11 s') pe ]
      it "CM463" $ do
        let s  = "[link](foo\nbar)"
            s' = s <> "\n"
            pe = utok '\n' <> etok '>' <> elabel "escaped link character"
              <> elabel "unescaped link character"
        s  ~-> [ err (posN 9 s)  pe ]
        s' ~-> [ err (posN 9 s') pe ]
      it "CM464" $ do
        let s  = "[link](<foo\nbar>)"
            s' = "<p>[link](<foo\nbar>)</p>\n"
            pe = utok '\n' <> etok '>' <> elabel "escaped link character"
              <> elabel "unescaped link character"
        s  ~-> [ err (posN 11 s)  pe ]
        s' ~-> [ err (posN 11 s') pe ]
      it "CM465" $
        "[link](\\(foo\\))" ==->
          "<p><a href=\"(foo)\">link</a></p>\n"
      it "CM466" $ do
        let s  = "[link](foo(and(bar)))"
            s' = s <> "\n"
            pe = utok '(' <> etok ')' <> elabel "escaped link character"
              <> elabel "unescaped link character"
        s  ~-> [ err (posN 10 s)  pe ]
        s' ~-> [ err (posN 10 s') pe ]
      it "CM467" $
        "[link](foo\\(and\\(bar\\))" ==->
          "<p><a href=\"foo(and(bar)\">link</a></p>\n"
      it "CM468" $
        "[link](<foo(and(bar)>)" ==->
          "<p><a href=\"foo(and(bar)\">link</a></p>\n"
      it "CM469" $
        "[link](foo\\)\\:)" ==->
          "<p><a href=\"foo):\">link</a></p>\n"
      it "CM470" $
        "[link](#fragment)\n\n[link](http://example.com#fragment)\n\n[link](http://example.com?foo=3#frag)\n"
          ==-> "<p><a href=\"#fragment\">link</a></p>\n<p><a href=\"http://example.com#fragment\">link</a></p>\n<p><a href=\"http://example.com?foo=3#frag\">link</a></p>\n"
      it "CM471" $
        "[link](foo\\bar)"
          ==-> "<p><a href=\"foo%5Cbar\">link</a></p>\n"
      it "CM472" $
        "[link](foo%20b&auml;)"
          ==-> "<p><a href=\"foo%20b%C3%A4\">link</a></p>\n"
      it "CM473" $
        "[link](\"title\")"
          ==-> "<p><a href=\"%22title%22\">link</a></p>\n"
    context "6.9 Hard line breaks" $ do
      -- NOTE We currently do not support hard line breaks represented in
      -- markup as space before newline.
      xit "CM603" $
        "foo  \nbaz" ==-> "<p>foo<br>\nbaz</p>\n"
      it "CM604" $
        "foo\\\nbaz\n" ==-> "<p>foo<br>\nbaz</p>\n"
      xit "CM605" $
         "foo       \nbaz" ==-> "<p>foo<br>\nbaz</p>\n"
      xit "CM606" $
        "foo  \n     bar" ==-> "<p>foo<br>\nbar</p>\n"
      it "CM607" $
        "foo\\\n     bar" ==-> "<p>foo<br>\nbar</p>\n"
      xit "CM608" $
        "*foo  \nbar*" ==-> "<p><em>foo<br>\nbar</em></p>\n"
      it "CM609" $
        "*foo\\\nbar*" ==-> "<p><em>foo<br>\nbar</em></p>\n"
      it "CM610" $
        "`code  \nspan`" ==-> "<p><code>code span</code></p>\n"
      it "CM611" $
        "`code\\\nspan`" ==-> "<p><code>code\\ span</code></p>\n"
      xit "CM612" $
        "<a href=\"foo  \nbar\">" ==-> "<p><a href=\"foo  \nbar\"></p>\n"
      xit "CM613" $ -- FIXME peding HTML inlines
        "<a href=\"foo\\\nbar\">" ==-> "<p><a href=\"foo\\\nbar\"></p>\n"
      it "CM614" $
        "foo\\" ==-> "<p>foo\\</p>\n"
      xit "CM615" $
        "foo  " ==-> "<p>foo</p>\n"
      it "CM616" $
        "### foo\\" ==-> "<h3>foo\\</h3>\n"
      it "CM617" $
        "### foo  " ==-> "<h3>foo</h3>\n"
    context "6.10 Soft line breaks" $ do
      it "CM618" $
        "foo\nbaz" ==-> "<p>foo\nbaz</p>\n"
      it "CM619" $
        "foo \n baz" ==-> "<p>foo\nbaz</p>\n"
    context "6.11 Textual content" $ do
      it "CM620" $
        "hello $.;'there" ==-> "<p>hello $.;&#39;there</p>\n"
      it "CM621" $
        "Foo χρῆν" ==-> "<p>Foo χρῆν</p>\n"
      it "CM622" $
        "Multiple     spaces" ==-> "<p>Multiple     spaces</p>\n"
    context "multiple parse errors" $
      it "they are reported in correct order" $ do
        let s = "Foo `\n\nBar `.\n"
            pe = ueib <> etok '`' <> elabel "code span content"
        s ~->
          [ err (posN 5  s) pe
          , err (posN 13 s) pe
          ]
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

----------------------------------------------------------------------------
-- Helpers

-- | Unexpected end of inline block.

ueib :: Ord t => ET t
ueib = ulabel "end of inline block"
