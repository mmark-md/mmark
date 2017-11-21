{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMarkSpec (spec) where

import Data.Aeson
import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.MMark (MMarkErr (..))
import Text.MMark.Extension (Inline (..))
import Text.MMark.TestUtils
import Text.Megaparsec (ErrorFancy (..))
import qualified Control.Foldl        as L
import qualified Data.List.NonEmpty   as NE
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
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
      it "CM4" $
        "  - foo\n\n\tbar" ==->
          "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      it "CM5" $
        "- foo\n\n\t\tbar" ==->
          "<ul>\n<li>\n<p>foo</p>\n<pre><code>  bar\n</code></pre>\n</li>\n</ul>\n"
      it "CM6" $
        ">\t\tfoo" ==->
          "<blockquote>\n<pre><code>  foo\n</code></pre>\n</blockquote>\n"
      it "CM7" $
        "-\t\tfoo" ==->
          "<ul>\n<li>\n<pre><code>  foo\n</code></pre>\n</li>\n</ul>\n"
      it "CM8" $
        "    foo\n\tbar" ==->
          "<pre><code>foo\nbar\n</code></pre>\n"
      it "CM9" $
        " - foo\n   - bar\n\t - baz" ==->
          "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>baz</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
      it "CM10" $
        "#\tFoo" ==-> "<h1 id=\"foo\">Foo</h1>\n"
      it "CM11" $
        "*\t*\t*\t" ==-> "<hr>\n"
    context "3.1 Precedence" $
      it "CM12" $
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
        let s = "--\n**\n__\n"
        in s ~-> errFancy (posN 4 s) (nonFlanking "*")
      it "CM17" $
        " ***\n  ***\n   ***" ==-> "<hr>\n<hr>\n<hr>\n"
      it "CM18" $
        "    ***" ==-> "<pre><code>***\n</code></pre>\n"
      it "CM19" $
        let s = "Foo\n    ***\n"
        in s ~-> errFancy (posN 10 s)  (nonFlanking "*")
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
        let s = "_ _ _ _ a\n\na------\n\n---a---\n"
        in s ~-> errFancy posI (nonFlanking "_")
      it "CM26" $
        " *\\-*" ==-> "<p><em>-</em></p>\n"
      it "CM27" $
        "- foo\n***\n- bar" ==->
         "<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>\n"
      it "CM28" $
        let s = "Foo\n***\nbar"
        in s ~-> errFancy (posN 6 s) (nonFlanking "*")
      xit "CM29" $ -- FIXME pending setext headings
        "Foo\n---\nbar" ==->
          "<h2>Foo</h2>\n<p>bar</p>\n"
      it "CM30" $
        "* Foo\n* * *\n* Bar" ==->
          "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n<ul>\n<li>Bar</li>\n</ul>\n"
      it "CM31" $
        "- Foo\n- * * *" ==->
          "<ul>\n<li>Foo</li>\n<li>\n<hr />\n</li>\n</ul>\n"
    context "4.2 ATX headings" $ do
      it "CM32" $
        "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo" ==->
          "<h1 id=\"foo\">foo</h1>\n<h2 id=\"foo\">foo</h2>\n<h3 id=\"foo\">foo</h3>\n<h4 id=\"foo\">foo</h4>\n<h5 id=\"foo\">foo</h5>\n<h6 id=\"foo\">foo</h6>\n"
      it "CM33" $
        let s = "####### foo"
        in s ~-> err (posN 6 s) (utok '#' <> elabel "white space")
      it "CM34" $
        let s = "#5 bolt\n\n#hashtag"
        in s ~~->
             [ err (posN 1 s)  (utok '5' <> etok '#' <> elabel "white space")
             , err (posN 10 s) (utok 'h' <> etok '#' <> elabel "white space") ]
      it "CM35" $
        "\\## foo" ==-> "<p>## foo</p>\n"
      it "CM36" $
        "# foo *bar* \\*baz\\*" ==-> "<h1 id=\"foo-bar-baz\">foo <em>bar</em> *baz*</h1>\n"
      it "CM37" $
        "#                  foo                     " ==->
          "<h1 id=\"foo\">foo</h1>\n"
      it "CM38" $
        " ### foo\n  ## foo\n   # foo" ==->
          "<h3 id=\"foo\">foo</h3>\n<h2 id=\"foo\">foo</h2>\n<h1 id=\"foo\">foo</h1>\n"
      it "CM39" $
        "    # foo" ==-> "<pre><code># foo\n</code></pre>\n"
      it "CM40" $
        "foo\n    # bar" ==-> "<p>foo\n# bar</p>\n"
      it "CM41" $
        "## foo ##\n  ###   bar    ###" ==->
          "<h2 id=\"foo\">foo</h2>\n<h3 id=\"bar\">bar</h3>\n"
      it "CM42" $
        "# foo ##################################\n##### foo ##" ==->
          "<h1 id=\"foo\">foo</h1>\n<h5 id=\"foo\">foo</h5>\n"
      it "CM43" $
        "### foo ###     " ==-> "<h3 id=\"foo\">foo</h3>\n"
      it "CM44" $
        "### foo ### b" ==-> "<h3 id=\"foo-b\">foo ### b</h3>\n"
      it "CM45" $
        "# foo#" ==-> "<h1 id=\"foo\">foo#</h1>\n"
      it "CM46" $
        "### foo \\###\n## foo #\\##\n# foo \\#" ==->
          "<h3 id=\"foo\">foo ###</h3>\n<h2 id=\"foo\">foo ###</h2>\n<h1 id=\"foo\">foo #</h1>\n"
      it "CM47" $
        "****\n## foo\n****" ==->
          "<hr>\n<h2 id=\"foo\">foo</h2>\n<hr>\n"
      it "CM48" $
        "Foo bar\n# baz\nBar foo" ==->
          "<p>Foo bar\n# baz\nBar foo</p>\n"
      it "CM49" $
        let s = "## \n#\n### ###"
        in s ~~->
             [ err (posN 3 s) (utok '\n' <> elabel "heading character" <> elabel "white space")
             , err (posN 5 s) (utok '\n' <> etok '#' <> elabel "white space") ]
    context "4.4 Indented code blocks" $ do
      it "CM76" $
        "    a simple\n      indented code block" ==->
          "<pre><code>a simple\n  indented code block\n</code></pre>\n"
      it "CM77" $
        "  - foo\n\n    bar" ==->
          "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
      it "CM78" $
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
      it "CM94" $
        let s = "```"
        in s ~-> err (posN 3 s)
           (ueof <> etok '`' <> elabel "info string" <> elabel "newline")
      it "CM95" $
        let s = "`````\n\n```\naaa\n"
        in s ~-> err (posN 15 s)
           (ueof <> elabel "closing code fence" <> elabel "code block content")
      it "CM96" $
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
      it "CM105" $
        let s  = "```\naaa\n    ```\n"
        in s ~-> err (posN 16 s)
           (ueof <> elabel "closing code fence" <> elabel "code block content")
      it "CM106" $
        "``` ```\naaa" ==->
          "<p><code></code>\naaa</p>\n"
      it "CM107" $
        let s = "~~~~~~\naaa\n~~~ ~~\n"
        in s ~-> err (posN 18 s)
           (ueof <> elabel "closing code fence" <> elabel "code block content")
      it "CM108" $
        "foo\n```\nbar\n```\nbaz" ==->
          "<p>foo\n<code>bar</code>\nbaz</p>\n"
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
          "<p>aaa</p>\n<h1 id=\"aaa\">aaa</h1>\n"
    context "5.1 Block quotes" $ do
      it "CM189" $
        "> # Foo\n> bar\n> baz\n" ==->
          "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM190" $
        "># Foo\n>bar\n> baz\n" ==->
          "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM191" $
        "   > # Foo\n   > bar\n > baz\n" ==->
          "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM192" $
        "    > # Foo\n    > bar\n    > baz\n" ==->
          "<pre><code>&gt; # Foo\n&gt; bar\n&gt; baz\n</code></pre>\n"
      it "CM193" $
        "> # Foo\n> bar\nbaz\n" ==->
          "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
      it "CM194" $
        "> bar\nbaz\n> foo\n" ==->
          "<blockquote>\n<p>bar\nbaz\nfoo</p>\n</blockquote>\n"
      it "CM195" $
        "> foo\n---\n" ==->
          "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr />\n"
      it "CM196" $
        "> - foo\n- bar\n" ==->
          "<blockquote>\n<ul>\n<li>foo</li>\n</ul>\n</blockquote>\n<ul>\n<li>bar</li>\n</ul>\n"
      it "CM197" $
        ">     foo\n    bar\n" ==->
          "<blockquote>\n<pre><code>foo\n</code></pre>\n</blockquote>\n<pre><code>bar\n</code></pre>\n"
      it "CM198" $
        "> ```\nfoo\n```\n" ==->
          "<blockquote>\n<pre><code></code></pre>\n</blockquote>\n<p>foo</p>\n<pre><code></code></pre>\n"
      it "CM199" $
        "> foo\n    - bar\n" ==->
          "<blockquote>\n<p>foo\n- bar</p>\n</blockquote>\n"
      it "CM200" $
        ">\n" ==->
          "<blockquote>\n</blockquote>\n"
      it "CM201" $
        ">\n>  \n> \n" ==->
          "<blockquote>\n</blockquote>\n"
      it "CM202" $
        ">\n> foo\n>  \n" ==->
          "<blockquote>\n<p>foo</p>\n</blockquote>\n"
      it "CM203" $
        "> foo\n\n> bar\n" ==->
          "<blockquote>\n<p>foo</p>\n</blockquote>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
      it "CM204" $
        "> foo\n> bar\n" ==->
          "<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n"
    context "5.2 List items" $ do
      it "CM214" $
        "A paragraph\nwith two lines.\n\n    indented code\n\n> A block quote.\n" ==->
          "<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n"
      it "CM215" $
        "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote.\n" ==->
          "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
      it "CM216" $
        "- one\n\n two\n" ==->
          "<ul>\n<li>one</li>\n</ul>\n<p>two</p>\n"
      it "CM217" $
        "- one\n\n  two\n" ==->
          "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"
      it "CM218" $
        " -    one\n\n     two\n" ==->
          "<ul>\n<li>one</li>\n</ul>\n<pre><code> two\n</code></pre>\n"
      it "CM219" $
        " -    one\n\n      two\n" ==->
          "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"
      it "CM220" $
        "   > > 1.  one\n>>\n>>     two\n" ==->
          "<blockquote>\n<blockquote>\n<ol>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ol>\n</blockquote>\n</blockquote>\n"
    context "6 Inlines" $
      it "CM286" $
        let s  = "`hi`lo`\n"
        in s ~-> err (posN 7 s) (ueib <> etok '`' <> elabel "code span content")
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
        let s = "\\\\*emphasis*"
        in s ~-> err (posN 2 s) (utok '*' <> eeib <> eric)
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
      it "CM295" $
        "<http://example.com?find=*>" ==->
          "<p><a href=\"http://example.com/?find=*\">http://example.com/?find=*</a></p>\n"
      xit "CM296" $ -- FIXME pending HTML inlines
        "<a href=\"/bar\\/)\">" ==->
          "<p>&lt;a href=&quot;/bar/)&quot;&gt;</p>\n"
      it "CM297" $
        let s = "[foo](/bar\\* \"ti\\*tle\")"
        in s ~-> err (posN 10 s)
          (utok '\\' <> etok '#' <> etok '/' <> etok '?' <> euri <> eppi)
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
        let s  = "`foo\\`bar`\n"
        in s ~-> err (posN 10 s) (ueib <> etok '`' <> elabel "code span content")
      it "CM320" $
        let s  = "*foo`*`\n"
        in s ~-> err (posN 7 s) (ueib <> etok '*' <> eic)
      it "CM321" $
        let s = "[not a `link](/foo`)\n"
        in s ~-> err (posN 20 s) (ueib <> etok ']' <> eic <> eric)
      it "CM322" $
        let s = "`<a href=\"`\">`\n"
        in s ~-> err (posN 14 s) (ueib <> etok '`' <> elabel "code span content")
      xit "CM323" $ -- FIXME pending HTML inlines
        "<a href=\"`\">`" ==-> "<p><a href=\"`\">`</p>\n"
      it "CM324" $
        let s = "`<http://foo.bar.`baz>`\n"
        in s ~-> err (posN 23 s) (ueib <> etok '`' <> elabel "code span content")
      it "CM325" $
        "<http://foo.bar.`baz>`" ==->
          "<p>&lt;http://foo.bar.<code>baz></code></p>\n"
      it "CM326" $
        let s  = "```foo``\n"
        in s ~-> err (posN 8 s) (ueib <> etok '`' <> elabel "code span content")
      it "CM327" $
        let s = "`foo\n"
        in s ~-> err (posN 4 s) (ueib <> etok '`' <> elabel "code span content")
      it "CM328" $
        let s  = "`foo``bar``\n"
        in s ~-> err (posN 11 s) (ueib <> etok '`' <> elabel "code span content")
    context "6.4 Emphasis and strong emphasis" $ do
      it "CM329" $
        "*foo bar*" ==-> "<p><em>foo bar</em></p>\n"
      it "CM330" $
        let s = "a * foo bar*\n"
        in s ~-> err (posN 2 s) (utok '*' <> eeib <> eric)
      it "CM331" $
        let s = "a*\"foo\"*\n"
        in s ~-> err (posN 1 s) (utok '*' <> eeib <> eric)
      it "CM332" $
        let s = "* a *\n"
        in s  ~-> errFancy posI (nonFlanking "*")
      it "CM333" $
        let s = "foo*bar*\n"
        in s ~-> err (posN 3 s) (utok '*' <> eeib <> eric)
      it "CM334" $
        let s = "5*6*78\n"
        in s ~-> err (posN 1 s) (utok '*' <> eeib <> eric)
      it "CM335" $
        "_foo bar_" ==-> "<p><em>foo bar</em></p>\n"
      it "CM336" $
        let s = "_ foo bar_\n"
        in s ~-> errFancy posI (nonFlanking "_")
      it "CM337" $
        let s = "a_\"foo\"_\n"
        in s ~-> err (posN 1 s) (utok '_' <> eeib <> eric)
      it "CM338" $
        let s = "foo_bar_\n"
        in s  ~-> err (posN 3 s) (utok '_' <> eeib <> eric)
      it "CM339" $
        let s = "5_6_78\n"
        in s ~-> err (posN 1 s) (utok '_' <> eeib <> eric)
      it "CM340" $
        let s = "пристаням_стремятся_\n"
        in s ~-> err (posN 9 s) (utok '_' <> eeib <> eric)
      it "CM341" $
        let s  = "aa_\"bb\"_cc\n"
        in s ~-> err (posN 2 s) (utok '_' <> eeib <> eric)
      it "CM342" $
        let s  = "foo-_(bar)_\n"
        in s ~-> err (posN 4 s) (utok '_' <> eeib <> eric)
      it "CM343" $
        let s = "_foo*\n"
        in s ~-> err (posN 4 s) (utok '*' <> etok '_' <> eric)
      it "CM344" $
        let s = "*foo bar *\n"
        in s ~-> errFancy (posN 9 s) (nonFlanking "*")
      it "CM345" $
        let s = "*foo bar\n*\n"
        in s ~-> errFancy (posN 9 s) (nonFlanking "*")
      it "CM346" $
        let s = "*(*foo)\n"
        in s ~-> errFancy posI (nonFlanking "*")
      it "CM347" $
        let s = "*(*foo*)*\n"
        in s ~-> errFancy posI (nonFlanking "*")
      it "CM348" $
        let s = "*foo*bar\n"
        in s ~-> errFancy (posN 4 s) (nonFlanking "*")
      it "CM349" $
        let s = "_foo bar _\n"
        in s ~-> errFancy (posN 9 s) (nonFlanking "_")
      it "CM350" $
        let s = "_(_foo)\n"
        in s ~-> errFancy posI (nonFlanking "_")
      it "CM351" $
        let s = "_(_foo_)_\n"
        in s ~-> errFancy posI (nonFlanking "_")
      it "CM352" $
        let s = "_foo_bar\n"
        in s ~-> errFancy (posN 4 s) (nonFlanking "_")
      it "CM353" $
        let s = "_пристаням_стремятся\n"
        in s ~-> errFancy (posN 10 s) (nonFlanking "_")
      it "CM354" $
        let s = "_foo_bar_baz_\n"
        in s ~-> errFancy (posN 4 s) (nonFlanking "_")
      it "CM355" $
        "_\\(bar\\)_.\n" ==-> "<p><em>(bar)</em>.</p>\n"
      it "CM356" $
        "**foo bar**\n" ==-> "<p><strong>foo bar</strong></p>\n"
      it "CM357" $
        let s = "** foo bar**\n"
        in s ~-> errFancy (posN 1 s) (nonFlanking "*")
      it "CM358" $
        let s = "a**\"foo\"**\n"
        in s ~-> err (posN 1 s) (utok '*' <> eeib <> eric)
      it "CM359" $
        let s = "foo**bar**\n"
        in s ~-> err (posN 3 s) (utok '*' <> eeib <> eric)
      it "CM360" $
        "__foo bar__" ==-> "<p><strong>foo bar</strong></p>\n"
      it "CM361" $
        let s = "__ foo bar__\n"
        in s ~-> errFancy (posN 1 s) (nonFlanking "_")
      it "CM362" $
        let s = "__\nfoo bar__\n"
        in s ~-> errFancy (posN 1 s) (nonFlanking "_")
      it "CM363" $
        let s = "a__\"foo\"__\n"
        in s ~-> err (posN 1 s) (utok '_' <> eeib <> eric)
      it "CM364" $
        let s = "foo__bar__\n"
        in s ~-> err (posN 3 s) (utok '_' <> eeib <> eric)
      it "CM365" $
        let s = "5__6__78\n"
        in s ~-> err (posN 1 s) (utok '_' <> eeib <> eric)
      it "CM366" $
        let s = "пристаням__стремятся__\n"
        in s ~-> err (posN 9 s) (utok '_' <> eeib <> eric)
      it "CM367" $
        "__foo, __bar__, baz__" ==->
          "<p><strong>foo, <strong>bar</strong>, baz</strong></p>\n"
      it "CM368" $
        "foo-__\\(bar\\)__" ==-> "<p>foo-<strong>(bar)</strong></p>\n"
      it "CM369" $
        let s = "**foo bar **\n"
        in s ~-> errFancy (posN 11 s) (nonFlanking "*")
      it "CM370" $
        let s = "**(**foo)\n"
        in s ~-> errFancy (posN 1 s) (nonFlanking "*")
      it "CM371" $
        let s = "*(**foo**)*\n"
        in s ~-> errFancy posI (nonFlanking "*")
      xit "CM372" $ -- FIXME doesn't pass with current approach
        "**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**" ==->
        "<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.\n<em>Asclepias physocarpa</em>)</strong></p>\n"
      it "CM373" $
        "**foo \"*bar*\" foo**" ==->
          "<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>\n"
      it "CM374" $
        let s = "**foo**bar\n"
        in s ~-> errFancy (posN 5 s) (nonFlanking "**")
      it "CM375" $
        let s = "__foo bar __\n"
        in s ~-> errFancy (posN 11 s) (nonFlanking "_")
      it "CM376" $
        let s = "__(__foo)\n"
        in s ~-> errFancy (posN 1 s) (nonFlanking "_")
      it "CM377" $
        let s = "_(__foo__)_\n"
        in s ~-> errFancy posI (nonFlanking "_")
      it "CM378" $
        let s = "__foo__bar\n"
        in s ~-> errFancy (posN 5 s) (nonFlanking "__")
      it "CM379" $
        let s = "__пристаням__стремятся\n"
        in s ~-> errFancy (posN 11 s) (nonFlanking "__")
      it "CM380" $
        "__foo\\_\\_bar\\_\\_baz__" ==->
          "<p><strong>foo__bar__baz</strong></p>\n"
      it "CM381" $
        "__\\(bar\\)__." ==->
          "<p><strong>(bar)</strong>.</p>\n"
      it "CM382" $
        "*foo [bar](/url)*" ==->
          "<p><em>foo <a href=\"/url\">bar</a></em></p>\n"
      it "CM383" $
        "*foo\nbar*" ==->
          "<p><em>foo\nbar</em></p>\n"
      it "CM384" $
        "_foo __bar__ baz_" ==->
          "<p><em>foo <strong>bar</strong> baz</em></p>\n"
      it "CM385" $
        "_foo _bar_ baz_" ==->
          "<p><em>foo <em>bar</em> baz</em></p>\n"
      it "CM386" $
        let s = "__foo_ bar_"
        in s ~-> err (posN 5 s) (utoks "_ " <> etoks "__" <> eric)
      it "CM387" $
        "*foo *bar**" ==->
          "<p><em>foo <em>bar</em></em></p>\n"
      it "CM388" $
        "*foo **bar** baz*" ==->
          "<p><em>foo <strong>bar</strong> baz</em></p>\n"
      it "CM389" $
        let s = "*foo**bar**baz*\n"
        in s ~-> err (posN 5 s) (utok '*' <> eeib)
      it "CM390" $
        "***foo** bar*\n" ==-> "<p><em><strong>foo</strong> bar</em></p>\n"
      it "CM391" $
        "*foo **bar***\n" ==-> "<p><em>foo <strong>bar</strong></em></p>\n"
      it "CM392" $
        let s = "*foo**bar***\n"
        in s ~-> err (posN 5 s) (utok '*' <> elabel "end of inline block")
      it "CM393" $
        "*foo **bar *baz* bim** bop*\n" ==->
          "<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>\n"
      it "CM394" $
        "*foo [*bar*](/url)*\n" ==->
          "<p><em>foo <a href=\"/url\"><em>bar</em></a></em></p>\n"
      it "CM395" $
        let s = "** is not an empty emphasis\n"
        in s ~-> errFancy (posN 1 s) (nonFlanking "*")
      it "CM396" $
        let s = "**** is not an empty strong emphasis\n"
        in s ~-> errFancy (posN 3 s) (nonFlanking "*")
      it "CM397" $
        "**foo [bar](/url)**" ==->
          "<p><strong>foo <a href=\"/url\">bar</a></strong></p>\n"
      it "CM398" $
        "**foo\nbar**" ==->
          "<p><strong>foo\nbar</strong></p>\n"
      it "CM399" $
        "__foo _bar_ baz__" ==->
          "<p><strong>foo <em>bar</em> baz</strong></p>\n"
      it "CM400" $
        "__foo __bar__ baz__" ==->
          "<p><strong>foo <strong>bar</strong> baz</strong></p>\n"
      it "CM401" $
        "____foo__ bar__" ==->
          "<p><strong><strong>foo</strong> bar</strong></p>\n"
      it "CM402" $
        "**foo **bar****" ==->
          "<p><strong>foo <strong>bar</strong></strong></p>\n"
      it "CM403" $
        "**foo *bar* baz**" ==->
          "<p><strong>foo <em>bar</em> baz</strong></p>\n"
      it "CM404" $
        let s = "**foo*bar*baz**\n"
        in s ~-> err (posN 5 s) (utoks "*b" <> etoks "**" <> eric)
      it "CM405" $
        "***foo* bar**" ==->
          "<p><strong><em>foo</em> bar</strong></p>\n"
      it "CM406" $
        "**foo *bar***" ==->
          "<p><strong>foo <em>bar</em></strong></p>\n"
      it "CM407" $
        "**foo *bar **baz**\nbim* bop**" ==->
          "<p><strong>foo <em>bar <strong>baz</strong>\nbim</em> bop</strong></p>\n"
      it "CM408" $
        "**foo [*bar*](/url)**" ==->
          "<p><strong>foo <a href=\"/url\"><em>bar</em></a></strong></p>\n"
      it "CM409" $
        let s = "__ is not an empty emphasis\n"
        in s ~-> errFancy (posN 1 s) (nonFlanking "_")
      it "CM410" $
        let s = "____ is not an empty strong emphasis\n"
        in s ~-> errFancy (posN 3 s) (nonFlanking "_")
      it "CM411" $
        let s = "foo ***\n"
        in s ~-> errFancy (posN 6 s) (nonFlanking "*")
      it "CM412" $
        "foo *\\**" ==-> "<p>foo <em>*</em></p>\n"
      it "CM413" $
        "foo *\\_*\n" ==-> "<p>foo <em>_</em></p>\n"
      it "CM414" $
        let s = "foo *****\n"
        in s ~-> errFancy (posN 8 s) (nonFlanking "*")
      it "CM415" $
        "foo **\\***" ==-> "<p>foo <strong>*</strong></p>\n"
      it "CM416" $
        "foo **\\_**\n" ==-> "<p>foo <strong>_</strong></p>\n"
      it "CM417" $
        let s = "**foo*\n"
        in s ~-> err (posN 5 s) (utok '*' <> etoks "**" <> eric)
      it "CM418" $
        let s = "*foo**\n"
        in s ~-> err (posN 5 s) (utok '*' <> eeib)
      it "CM419" $
        let s = "***foo**\n"
        in s ~-> err (posN 8 s) (ueib <> etok '*' <> eic)
      it "CM420" $
        let s = "****foo*\n"
        in s ~-> err (posN 7 s) (utok '*' <> etoks "**" <> eric)
      it "CM421" $
        let s = "**foo***\n"
        in s ~-> err (posN 7 s) (utok '*' <> eeib)
      it "CM422" $
        let s = "*foo****\n"
        in s ~-> err (posN 5 s) (utok '*' <> eeib)
      it "CM423" $
        let s = "foo ___\n"
        in s ~-> errFancy (posN 6 s) (nonFlanking "_")
      it "CM424" $
        "foo _\\__" ==-> "<p>foo <em>_</em></p>\n"
      it "CM425" $
        "foo _\\*_" ==-> "<p>foo <em>*</em></p>\n"
      it "CM426" $
        let s = "foo _____\n"
        in s ~-> errFancy (posN 8 s) (nonFlanking "_")
      it "CM427" $
        "foo __\\___" ==-> "<p>foo <strong>_</strong></p>\n"
      it "CM428" $
        "foo __\\*__" ==-> "<p>foo <strong>*</strong></p>\n"
      it "CM429" $
        let s = "__foo_\n"
        in s ~-> err (posN 5 s) (utok '_' <> etoks "__" <> eric)
      it "CM430" $
        let s = "_foo__\n"
        in s ~-> err (posN 5 s) (utok '_' <> eeib)
      it "CM431" $
        let s = "___foo__\n"
        in s ~-> err (posN 8 s) (ueib <> etok '_' <> eic)
      it "CM432" $
        let s = "____foo_\n"
        in s ~-> err (posN 7 s) (utok '_' <> etoks "__" <> eric)
      it "CM433" $
        let s = "__foo___\n"
        in s ~-> err (posN 7 s) (utok '_' <> eeib)
      it "CM434" $
        let s = "_foo____\n"
        in s ~-> err (posN 5 s) (utok '_' <> eeib)
      it "CM435" $
        "**foo**" ==-> "<p><strong>foo</strong></p>\n"
      it "CM436" $
        "*_foo_*" ==-> "<p><em><em>foo</em></em></p>\n"
      it "CM437" $
        "__foo__" ==-> "<p><strong>foo</strong></p>\n"
      it "CM438" $
        "_*foo*_" ==-> "<p><em><em>foo</em></em></p>\n"
      it "CM439" $
        "****foo****" ==-> "<p><strong><strong>foo</strong></strong></p>\n"
      it "CM440" $
        "____foo____" ==-> "<p><strong><strong>foo</strong></strong></p>\n"
      it "CM441" $
        "******foo******" ==->
          "<p><strong><strong><strong>foo</strong></strong></strong></p>\n"
      it "CM442" $
        "***foo***" ==-> "<p><em><strong>foo</strong></em></p>\n"
      it "CM443" $
        "_____foo_____" ==->
          "<p><strong><strong><em>foo</em></strong></strong></p>\n"
      it "CM444" $
        let s = "*foo _bar* baz_\n"
        in s ~-> err (posN 9 s) (utok '*' <> etok '_' <> eric)
      it "CM445" $
        let s = "*foo __bar *baz bim__ bam*\n"
        in s ~-> err (posN 19 s) (utok '_' <> etok '*' <> eric)
      it "CM446" $
        let s = "**foo **bar baz**\n"
        in s ~-> err (posN 17 s) (ueib <> etoks "**" <> eic)
      it "CM447" $
        let s = "*foo *bar baz*\n"
        in s ~-> err (posN 14 s) (ueib <> etok '*' <> eic)
      it "CM448" $
        let s = "*[bar*](/url)\n"
        in s ~-> err (posN 5 s) (utok '*' <> etok ']' <> eric)
      it "CM449" $
        let s = "_foo [bar_](/url)\n"
        in s ~-> err (posN 9 s) (utok '_' <> etok ']' <> eric)
      xit "CM450" $ -- FIXME pending HTML inlines
        "*<img src=\"foo\" title=\"*\"/>" ==->
          "<p>*<img src=\"foo\" title=\"*\"/></p>\n"
      xit "CM451" $ -- FIXME pending HTML inlines
        "**<a href=\"**\">" ==-> "<p>**<a href=\"**\"></p>\n"
      xit "CM452" $
        "__<a href=\"__\">\n" ==-> "<p>__<a href=\"__\"></p>\n"
      it "CM453" $
        "*a `*`*" ==-> "<p><em>a <code>*</code></em></p>\n"
      it "CM454" $
        "_a `_`_" ==-> "<p><em>a <code>_</code></em></p>\n"
      it "CM455" $
        let s = "**a<http://foo.bar/?q=**>"
        in s ~-> err (posN 25 s) (ueib <> etoks "**" <> eic)
      it "CM456" $
        let s = "__a<http://foo.bar/?q=__>"
        in s ~-> err (posN 26 s) (ueib <> etoks "__" <> eic)
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
        let s = "[link](/my uri)\n"
        in s ~-> err (posN 11 s)
           (utok 'u' <> etok '"' <> etok '\'' <> etok '(' <> elabel "white space")
      it "CM462" $
        let s = "[link](</my uri>)\n"
        in s ~-> err (posN 11 s)
           (utok ' ' <> etok '#' <> etok '/' <> etok '>' <> etok '?' <> eppi)
      it "CM463" $
        let s = "[link](foo\nbar)\n"
        in s ~-> err (posN 11 s)
           (utok 'b' <> etok '"' <> etok '\'' <> etok '(' <> elabel "white space")
      it "CM464" $
        let s = "[link](<foo\nbar>)\n"
        in s ~-> err (posN 11 s)
           (utok '\n' <> etok '#' <> etok '/' <> etok '>' <> etok '?' <> eppi)
      it "CM465" $
        let s = "[link](\\(foo\\))"
        in s ~-> err (posN 7 s) (utok '\\' <> etoks "//" <> etok '#' <>
             etok '/' <> etok '<' <> etok '?' <> elabel "ASCII alpha character" <>
             euri <> elabel "path piece")
      it "CM466" $
        "[link](foo(and(bar)))\n" ==->
          "<p><a href=\"foo(and(bar\">link</a>))</p>\n"
      it "CM467" $
        let s = "[link](foo\\(and\\(bar\\))"
        in s ~-> err (posN 10 s) (utok '\\' <> etok '#' <> etok '/' <> etok '?' <> euri <> eppi)
      it "CM468" $
        "[link](<foo(and(bar)>)" ==->
          "<p><a href=\"foo(and(bar)\">link</a></p>\n"
      it "CM469" $
        let s = "[link](foo\\)\\:)"
        in s ~-> err (posN 10 s) (utok '\\' <> etok '#' <> etok '/' <> etok '?' <> euri <> eppi)
      it "CM470" $
        "[link](#fragment)\n\n[link](http://example.com#fragment)\n\n[link](http://example.com?foo=3#frag)\n"
          ==-> "<p><a href=\"#fragment\">link</a></p>\n<p><a href=\"http://example.com/#fragment\">link</a></p>\n<p><a href=\"http://example.com/?foo=3#frag\">link</a></p>\n"
      it "CM471" $
        let s = "[link](foo\\bar)"
        in s ~-> err (posN 10 s) (utok '\\' <> etok '#' <> etok '/' <> etok '?' <> euri <> eppi)
      xit "CM472" $ -- FIXME pending entity references
        "[link](foo%20b&auml;)"
          ==-> "<p><a href=\"foo%20b&amp;auml;\">link</a></p>\n"
      it "CM473" $
        let s = "[link](\"title\")"
        in s ~-> err (posN 7 s)
             (utok '"' <> etoks "//" <> etok '#' <> etok '/' <> etok '<' <>
              etok '?' <> elabel "ASCII alpha character" <> euri <> elabel "path piece")
      it "CM474" $
        "[link](/url \"title\")\n[link](/url 'title')\n[link](/url (title))" ==->
          "<p><a href=\"/url\" title=\"title\">link</a>\n<a href=\"/url\" title=\"title\">link</a>\n<a href=\"/url\" title=\"title\">link</a></p>\n"
      xit "CM475" $ -- FIXME pending entity references
        "[link](/url \"title \\\"&quot;\")\n" ==->
          "<p><a href=\"/url\" title=\"title &quot;&quot;\">link</a></p>\n"
      it "CM476" $
        let s = "[link](/url \"title\")"
        in s ~-> err (posN 11 s)
             (utok ' ' <> etok '#' <> etok '/' <> etok '?' <> euri <> eppi)
      it "CM477" $
        let s = "[link](/url \"title \"and\" title\")\n"
        in s ~-> err (posN 20 s) (utok 'a' <> etok ')' <> elabel "white space")
      it "CM478" $
        "[link](/url 'title \"and\" title')" ==->
          "<p><a href=\"/url\" title=\"title &quot;and&quot; title\">link</a></p>\n"
      it "CM479" $
        "[link](   /uri\n  \"title\"  )" ==->
          "<p><a href=\"/uri\" title=\"title\">link</a></p>\n"
      it "CM480" $
        let s = "[link] (/uri)\n"
        in s ~-> err (posN 6 s) (utok ' ' <> etok '(')
      it "CM481" $
        let s = "[link [foo [bar]]](/uri)\n"
        in s ~-> err (posN 6 s) (utok '[' <> etok ']' <> eic <> eric)
      it "CM482" $
        let s = "[link] bar](/uri)\n"
        in s ~-> err (posN 6 s) (utok ' ' <> etok '(')
      it "CM483" $
        let s = "[link [bar](/uri)\n"
        in s ~-> err (posN 6 s) (utok '[' <> etok ']' <> eic <> eric)
      it "CM484" $
        "[link \\[bar](/uri)\n" ==->
          "<p><a href=\"/uri\">link [bar</a></p>\n"
      it "CM485" $
        "[link *foo **bar** `#`*](/uri)" ==->
          "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"
      it "CM486" $
        "[![moon](moon.jpg)](/uri)" ==->
          "<p><a href=\"/uri\"><img src=\"moon.jpg\" alt=\"moon\"></a></p>\n"
      it "CM487" $
        let s = "[foo [bar](/uri)](/uri)\n"
        in s ~-> err (posN 5 s) (utok '[' <> etok ']' <> eic <> eric)
      it "CM488" $
        let s = "[foo *[bar [baz](/uri)](/uri)*](/uri)\n"
        in s ~-> err (posN 11 s) (utok '[' <> etok ']' <> eic <> eric)
      it "CM489" $
        let s = "![[[foo](uri1)](uri2)](uri3)"
        in s ~-> err (posN 3 s) (utoks "[foo" <> eeib <> eic)
      it "CM490" $
        let s = "*[foo*](/uri)\n"
        in s ~-> err (posN 5 s) (utok '*' <> etok ']' <> eric)
      it "CM491" $
        let s = "[foo *bar](baz*)\n"
        in s ~-> err (posN 9 s) (utok ']' <> etok '*' <> eic <> eric)
      it "CM492" $
        let s = "*foo [bar* baz]\n"
        in s ~-> err (posN 9 s) (utok '*' <> etok ']' <> eric)
      xit "CM493" $ -- FIXME pending inline HTML
        let s = "[foo <bar attr=\"](baz)\">"
        in s ~-> err (posN 5 s) (utok '<' <> etok ']')
      it "CM494" $
        let s = "[foo`](/uri)`\n"
        in s ~-> err (posN 13 s) (ueib <> etok ']' <> eic)
      it "CM495" $
        "[foo<http://example.com/?search=](uri)>" ==->
          "<p><a href=\"uri\">foo&lt;http://example.com/?search=</a>&gt;</p>\n"
    context "6.6 Images" $ do
      it "CM541" $
        "![foo](/url \"title\")" ==->
          "<p><img src=\"/url\" title=\"title\" alt=\"foo\"></p>\n"
      it "CM542" $
        "![foo *bar*](train.jpg \"train & tracks\")" ==->
          "<p><img src=\"train.jpg\" title=\"train &amp; tracks\" alt=\"foo bar\"></p>\n"
      it "CM543" $
        let s = "![foo ![bar](/url)](/url2)\n"
        in s ~-> err (posN 6 s) (utok '!' <> etok ']')
      it "CM544" $
        "![foo [bar](/url)](/url2)" ==->
          "<p><img src=\"/url2\" alt=\"foo bar\"></p>\n"
      it "CM545" pending
      it "CM546" pending
      it "CM547" $
        "![foo](train.jpg)" ==->
          "<p><img src=\"train.jpg\" alt=\"foo\"></p>\n"
      it "CM548" $
        "My ![foo bar](/path/to/train.jpg  \"title\"   )" ==->
          "<p>My <img src=\"/path/to/train.jpg\" title=\"title\" alt=\"foo bar\"></p>\n"
      it "CM549" $
        "![foo](<url>)" ==->
          "<p><img src=\"url\" alt=\"foo\"></p>\n"
      it "CM550" $
        "![](/url)" ==-> "<p><img src=\"/url\" alt></p>\n"
      it "CM551-CM562" pending -- pending reference-style stuff
    context "6.7 Autolinks" $ do
      it "CM563" $
        "<http://foo.bar.baz>" ==->
          "<p><a href=\"http://foo.bar.baz/\">http://foo.bar.baz/</a></p>\n"
      it "CM564" $
        "<http://foo.bar.baz/test?q=hello&id=22&boolean>" ==->
          "<p><a href=\"http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean\">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>\n"
      it "CM565" $
        "<irc://foo.bar:2233/baz>" ==->
          "<p><a href=\"irc://foo.bar:2233/baz\">irc://foo.bar:2233/baz</a></p>\n"
      it "CM566" $
        "<MAILTO:FOO@BAR.BAZ>" ==->
          "<p><a href=\"mailto:FOO@BAR.BAZ\">FOO@BAR.BAZ</a></p>\n"
      it "CM567" $
        "<a+b+c:d>" ==->
          "<p><a href=\"a+b+c:d\">a+b+c:d</a></p>\n"
      it "CM568" $
        "<made-up-scheme://foo,bar>" ==->
          "<p><a href=\"made-up-scheme://foo/,bar\">made-up-scheme://foo/,bar</a></p>\n"
      it "CM569" $
        "<http://../>" ==->
          "<p>&lt;http://../&gt;</p>\n"
      it "CM570" $
        "<localhost:5001/foo>" ==->
          "<p><a href=\"localhost:5001/foo\">localhost:5001/foo</a></p>\n"
      it "CM571" $
        "<http://foo.bar/baz bim>\n" ==->
          "<p>&lt;http://foo.bar/baz bim&gt;</p>\n"
      it "CM572" $
        "<http://example.com/\\[\\>" ==->
          "<p>&lt;http://example.com/[&gt;</p>\n"
      it "CM573" $
        "<foo@bar.example.com>" ==->
          "<p><a href=\"mailto:foo@bar.example.com\">foo@bar.example.com</a></p>\n"
      it "CM574" $
        "<foo+special@Bar.baz-bar0.com>" ==->
          "<p><a href=\"mailto:foo+special@Bar.baz-bar0.com\">foo+special@Bar.baz-bar0.com</a></p>\n"
      it "CM575" $
        "<foo\\+@bar.example.com>" ==->
          "<p>&lt;foo+@bar.example.com&gt;</p>\n"
      it "CM576" $
        "<>" ==->
          "<p>&lt;&gt;</p>\n"
      it "CM577" $
        "< http://foo.bar >" ==->
          "<p>&lt; http://foo.bar &gt;</p>\n"
      it "CM578" $
        "<m:abc>" ==->
          "<p><a href=\"m:abc\">m:abc</a></p>\n"
      it "CM579" $
        "<foo.bar.baz>" ==->
          "<p><a href=\"foo.bar.baz\">foo.bar.baz</a></p>\n"
      it "CM580" $
        "http://example.com" ==->
          "<p>http://example.com</p>\n"
      it "CM581" $
        "foo@bar.example.com" ==->
          "<p>foo@bar.example.com</p>\n"
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
      xit "CM613" $ -- FIXME pending HTML inlines
        "<a href=\"foo\\\nbar\">" ==-> "<p><a href=\"foo\\\nbar\"></p>\n"
      it "CM614" $
        "foo\\" ==-> "<p>foo\\</p>\n"
      xit "CM615" $
        "foo  " ==-> "<p>foo</p>\n"
      it "CM616" $
        "### foo\\" ==-> "<h3 id=\"foo\">foo\\</h3>\n"
      it "CM617" $
        "### foo  " ==-> "<h3 id=\"foo\">foo</h3>\n"
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
    -- NOTE I don't test these so extensively because they share
    -- implementation with emphasis and strong emphasis which are thoroughly
    -- tested already.
    context "strikeout" $ do
      it "works in simplest form" $
        "It's ~~bad~~ news." ==->
          "<p>It&#39;s <del>bad</del> news.</p>\n"
      it "combines with emphasis" $
        "**It's ~~bad~~** news." ==->
          "<p><strong>It&#39;s <del>bad</del></strong> news.</p>\n"
      it "interacts with subscript reasonably (1)" $
        "It's ~~~bad~~ news~." ==->
          "<p>It&#39;s <sub><del>bad</del> news</sub>.</p>\n"
      it "interacts with subscript reasonably (2)" $
        "It's ~~~bad~ news~~." ==->
          "<p>It&#39;s <del><sub>bad</sub> news</del>.</p>\n"
    context "subscript" $ do
      it "works in simplest form" $
        "It's ~bad~ news." ==->
          "<p>It&#39;s <sub>bad</sub> news.</p>\n"
      it "combines with emphasis" $
        "**It's ~bad~** news." ==->
          "<p><strong>It&#39;s <sub>bad</sub></strong> news.</p>\n"
    context "superscript" $ do
      it "works in simplest form" $
        "It's ^bad^ news." ==->
          "<p>It&#39;s <sup>bad</sup> news.</p>\n"
      it "combines with emphasis" $
        "**It's ^bad^** news." ==->
          "<p><strong>It&#39;s <sup>bad</sup></strong> news.</p>\n"
      it "a composite, complex example" $
        "***Something ~~~is not~~ going~ ^so well^** today*." ==->
          "<p><em><strong>Something <sub><del>is not</del> going</sub> <sup>so well</sup></strong> today</em>.</p>\n"
    context "multiple parse errors" $ do
      it "they are reported in correct order" $ do
        let s = "Foo `\n\nBar `.\n"
            pe = ueib <> etok '`' <> elabel "code span content"
        s ~~->
          [ err (posN 5  s) pe
          , err (posN 13 s) pe ]
      it "invalid headers are skipped properly" $ do
        let s = "#My header\n\nSomething goes __here __.\n"
        s ~~->
          [ err (posN 1 s) (utok 'M' <> etok '#' <> elabel "white space")
          , errFancy (posN 35 s) (nonFlanking "_") ]
    context "given a complete, comprehensive document" $
      it "outputs expected the HTML fragment" $
        withFiles "data/comprehensive.md" "data/comprehensive.html"
  describe "parseErrorsPretty" $
    it "renders parse errors correctly" $ do
      let s = "Foo\nBar\nBaz\n"
          e0 = err posI       (utok 'F' <> etok 'Z')
          e1 = err (posN 4 s) (utok 'B' <> etok 'Z')
          e2 = err (posN 8 s) (utok 'B' <> etok 'Z')
      MMark.parseErrorsPretty s (e0:|[e1,e2]) `shouldBe`
        "1:1:\n  |\n1 | Foo\n  | ^\nunexpected 'F'\nexpecting 'Z'\n2:1:\n  |\n2 | Bar\n  | ^\nunexpected 'B'\nexpecting 'Z'\n3:1:\n  |\n3 | Baz\n  | ^\nunexpected 'B'\nexpecting 'Z'\n"
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
      let n = MMark.runScanner doc (length_scan (const True))
      n `shouldBe` 17
  describe "combining of scanners" $
    it "combines scanners" $ do
      doc <- mkDoc "Here we go, pals."
      let scan = (,,)
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
      context "when it is valid" $
        it "returns the YAML section" $ do
          doc <- mkDoc "---\nx: 100\ny: 200\n---Here we go."
          let r = object
                [ "x" .= Number 100
                , "y" .= Number 200 ]
          MMark.projectYaml doc `shouldBe` Just r
      context "when it is invalid" $
        it "signal correct parse error" $
          let s = "---\nx: 100\ny: x:\n---Here we go."
          in s ~-> errFancy (posN 15 s)
             (fancy . ErrorCustom . YamlParseError $
              "mapping values are not allowed in this context")

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

length_scan :: (Char -> Bool) -> L.Fold (Ext.Block (NonEmpty Inline)) Int
length_scan p = Ext.scanner 0 $ \n block ->
  getSum $ Sum n <> foldMap (foldMap f) block
  where
    f (Plain txt) = (Sum . T.length) (T.filter p txt)
    f _           = mempty

----------------------------------------------------------------------------
-- For testing with documents loaded externally

-- | Load a complete markdown document from an external file and compare the
-- final HTML rendering with contents of another file.

withFiles
  :: FilePath          -- ^ Markdown document
  -> FilePath          -- ^ HTML document containing the correct result
  -> Expectation
withFiles input output = do
  i <- TIO.readFile input
  o <- TIO.readFile output
  i ==-> o

----------------------------------------------------------------------------
-- Helpers

-- | Unexpected end of inline block.

ueib :: Ord t => ET t
ueib = ulabel "end of inline block"

-- | Expecting end of inline block.

eeib :: Ord t => ET t
eeib = elabel "end of inline block"

-- | Expecting end of URI literal.

euri :: Ord t => ET t
euri = elabel "end of URI literal"

-- | Expecting the rest of path piece.

eppi :: Ord t => ET t
eppi = elabel "the rest of path piece"

-- | Expecting inline content.

eic :: Ord t => ET t
eic = elabel "inline content"

-- | Expecting rest of inline content. Eric!

eric :: Ord t => ET t
eric = elabel "the rest of inline content"

-- | Create a error component complaining that the given 'Text' is not in
-- left- or right- flanking position.

nonFlanking :: Text -> EF MMarkErr
nonFlanking = fancy . ErrorCustom . NonFlankingDelimiterRun . NE.fromList . T.unpack
