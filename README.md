# MMark

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/mmark.svg?style=flat)](https://hackage.haskell.org/package/mmark)
[![Stackage Nightly](http://stackage.org/package/mmark/badge/nightly)](http://stackage.org/nightly/package/mmark)
[![Stackage LTS](http://stackage.org/package/mmark/badge/lts)](http://stackage.org/lts/package/mmark)
![CI](https://github.com/mmark-md/mmark/workflows/CI/badge.svg?branch=master)

* [Quick start: MMark vs GitHub-flavored markdown](#quick-start-mmark-vs-github-flavored-markdown)
* [MMark and Common Mark](#mmark-and-common-mark)
    * [Differences in inline parsing](#differences-in-inline-parsing)
    * [Other differences](#other-differences)
* [About MMark-specific extensions](#about-mmark-specific-extensions)
* [Performance](#performance)
* [Related packages](#related-packages)
* [Contribution](#contribution)
* [License](#license)

MMark (read “em-mark”) is a strict markdown processor for writers. “Strict”
means that not every input is considered valid markdown document and parse
errors are possible and even desirable, because they allow us to spot markup
issues without searching for them in rendered document. If a markdown
document passes the MMark parser, then it is likely to produce an HTML
output without quirks. This feature makes it a good choice for writers and
bloggers.

MMark in its current state features:

* A parser that produces high-quality error messages and does not choke on
  the first parse error. It is capable of reporting several parse errors
  simultaneously.

* An extension system that allows us to create extensions that alter parsed
  markdown document in some way.

* A [`lucid`](https://hackage.haskell.org/package/lucid)-based render.

There is also a blog post announcing the project:

https://markkarpov.com/post/announcing-mmark.html

## Quick start: MMark vs GitHub-flavored markdown

It's easy to start using MMark if you're used to GitHub-flavored markdown.
There are four main differences:

1. URIs are not automatically recognized, you must enclose them in `<` and
   `>`.

2. Block quotes require only one `>` and they continue as long as the inner
   content is indented.

   This is OK:

   ```
   > Here goes my block quote.
     And this is the second line of the quote.
   ```

   This produces *two* block quotes:

   ```
   > Here goes my block quote.
   > And this is another block quote!
   ```

3. HTML blocks and inline HTML are not supported.

4. See [differences in inline parsing](#differences-in-inline-parsing).

## MMark and Common Mark

MMark mostly tries to follow the Common Mark specification as given here:

https://spec.commonmark.org/0.28/

However, due to the fact that we do not allow inputs that do not make sense,
and also try to guard against common mistakes (like writing `##My header`
and having it rendered as a paragraph starting with hashes) MMark obviously
can't follow the specification precisely. In particular, parsing of inlines
differs considerably from Common Mark (see below).

Another difference between Common Mark and MMark is that the latter supports
more (pun alert) common markdown extensions out-of-the-box. In particular,
MMark supports:

* parsing of an optional YAML block
* strikeout using `~~this~~` syntax
* superscript using `^this^` syntax
* subscript using `~this~` syntax
* automatic assignment of ids to headers
* pipe tables (as on GitHub)

One does not need to enable or tweak anything for these to work, they are
built-in features.

### Differences in inline parsing

Emphasis and strong emphasis is an especially hairy topic in the Common Mark
specification. There are 17 ad-hoc rules defining the interaction between
`*` and `_` -based emphasis and more than an half of all Common Mark
examples (that's about 300) test just this.

Not only it is hard to implement, it's hard to understand for humans too.
For example, this input:

```
*(*foo*)*
```

results in the following HTML:

```
<p><em>(<em>foo</em>)</em></p>
```

(Note the nested emphasis.)

Could it produce something like this instead?

```
<p><em>(</em>foo<em>)</em></p>
```

Well, why not? Without remembering those 17 ad-hoc rules, there going to be
a lot of tricky cases when the user won't be able to tell how markdown will
be parsed.

I decided to make parsing of emphasis, strong emphasis, and similar
constructs like strikethrough, subscript, and superscript more symmetric and
less ad-hoc. In 99% of practical cases it is identical to Common Mark, and
normal markdown intuitions will work OK for the users.

Let's start by dividing all characters into four groups:

* **Space characters**, including space, tab, newline, carriage return, and
  other characters like non-breaking space.

* **Markup characters**, including the following: `*`, `~`, `_`, `` ` ``,
  `^`, `[`, `]`. These are used for markup and whenever they appear in a
  document, they must form valid markup constructions. To be used as
  ordinary punctuation characters they must be backslash escaped.

* **Punctuation characters**, which include all punctuation characters that
  are not **markup characters**.

* **Other characters**, which include all characters not falling into the
  three groups described above.

Next, let's assign *levels* to all groups but **markup characters**:

* **Space characters**—level 0
* **Punctuation characters**—level 1
* **Other characters**—level 2

When **markup characters** or **punctuation characters** are escaped with
backslash they become **other characters**.

We'll call **markdown characters** placed between a character of level `L`
and a character of level `R` *left-flanking delimiter run* if and only if:

```
level(L) < level(R)
```

These **markup characters** sort of hang on the left hand side of a word.

Similarly we'll call **markdown characters** placed between a character of
level `L` and a character of level `R` *right-flanking delimiter run* if and
only if:

```
level(L) > level (R)
```

These **markup characters** hang on the right hand side of a word.

*Emphasis markup* (and other similar things like strikethrough, which we
won't mention explicitly anymore for brevity) can start only as
*left-flanking delimiter run* and end only as *right-flanking delimiter
run*.

This produces a parse error:

```
*Something * is not right.
Something __is __ not right.
```

And this too:

```
__foo__bar
```

This means that inter-word emphasis is not supported.

The next example is OK because `s` is an **other character** and `.` is a
**punctuation character**, so `level('s') > level('.')`.

```
Here it *goes*.
```

In some rare cases backslash escaping can help get the right result:

```
Here goes *(something\)*.
```

We escaped the closing parenthesis `)` so it becomes an **other character**
with level 2 and so its level is greater than the level of plain punctuation
character `.`.

### Other differences

Block-level parsing:

* If a line starts with hash signs it is expected to be a valid *non-empty*
  header (level 1–6 inclusive). If you want to start a paragraph with
  hashes, just escape the first hash with backslash and that will be enough.
* Setext headings are not supported for the sake of simplicity.
* Fenced code blocks must be explicitly closed by a closing fence. They are
  not closed by the end of document or by start of another block.
* Lists and block quotes are defined by column at which their content
  starts. Content belonging to a particular list or block quote should start
  at the same column (or greater column, up to the column where indented
  code blocks start). As a consequence of this, block quotes do not feature
  “laziness”.
* Block quotes are started by a single `>` character, it's not necessary to
  put a `>` character at beginning of every line belonging to a quote (in
  fact, this would make every line a separate block quote).
* Paragraphs can be interrupted by unordered and ordered lists with any
  valid starting index.
* HTML blocks are not supported because the syntax conflicts with autolinks
  and the feature is a hack to compensate for the lack of extensibility and
  customization in the original markdown.

Inline-level parsing:

* MMark does not support hard line breaks represented as double space before
  newline. Nevertheless, hard line breaks in the form of backslash before
  newline are supported (these are more explicit too).
* All URI references (in links, images, autolinks, etc.) are parsed as per
  RFC 3986, no support for escaping or support for entity and numeric
  character references is provided. In addition to that, when a URI
  reference in not enclosed with `<` and `>`, then closing parenthesis
  character `)` is not considered part of URI (use `<uri>` syntax if you
  want a closing parenthesis as part of a URI). Since the empty string is a
  valid URI and it may be confusing in some cases, we also force the user to
  write `<>` to represent the empty URI.
* Putting links in text of another link is not allowed, i.e. no nested links
  is possible.
* Putting images in description of other images is not allowed (similarly to
  the situation with links).
* HTML inlines are not supported for the same reason why HTML blocks are not
  supported.

## About MMark-specific extensions

* YAML block must start with three hyphens `---` and end with three hyphens
  `---`. It can only be placed at the beginning of a markdown document.
  Trailing white space after the `---` sequences is allowed.

## Performance

I [have compared](https://github.com/mrkkrp/md-bench) speed and memory
consumption of various Haskell markdown libraries by running them on an
identical, big-enough markdown document and by rendering it as HTML:

Library             | Parsing library     | Execution time | Allocated   | Max residency
--------------------|---------------------|---------------:|------------:|-------------:
`cmark-0.5.6`       | Custom C code       |       323.4 μs |     228,440 |         9,608
`mmark-0.0.5.1`     | Megaparsec          |       7.027 ms |  26,180,272 |        37,792
`cheapskate-0.1.1`  | Custom Haskell code |       10.76 ms |  44,686,272 |       799,200
`markdown-0.1.16` † | Attoparsec          |       14.13 ms |  69,261,816 |       699,656
`pandoc-2.0.5`      | Parsec              |       37.90 ms | 141,868,840 |     1,471,080

*Results are ordered from fastest to slowest.*

† The `markdown` library is sloppy and parses markdown incorrectly. For
example, it parses the following `*My * text` as an inline containing
emphasis, while in reality both asterisks must form flanking delimiter runs
to create emphasis, like so `*My* text`. This allowed `markdown` to get away
with a far simpler approach to parsing at the price that it's not really a
valid markdown implementation.

## Related packages

* [`mmark-ext`](https://hackage.haskell.org/package/mmark-ext) contains some
  commonly useful MMark extensions.
* [`mmark-cli`](https://hackage.haskell.org/package/mmark-cli) is a command
  line interface to MMark.
* [`flycheck-mmark`](https://github.com/mmark-md/flycheck-mmark) is a way to
  check markdown documents against MMark parser interactively from Emacs.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mmark-md/mmark/issues).

Pull requests are also welcome.

## License

Copyright © 2017–present Mark Karpov

Distributed under BSD 3 clause license.
