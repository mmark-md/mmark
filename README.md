# MMark

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/mmark.svg?style=flat)](https://hackage.haskell.org/package/mmark)
[![Stackage Nightly](http://stackage.org/package/mmark/badge/nightly)](http://stackage.org/nightly/package/mmark)
[![Stackage LTS](http://stackage.org/package/mmark/badge/lts)](http://stackage.org/lts/package/mmark)
[![CI](https://github.com/mmark-md/mmark/actions/workflows/ci.yaml/badge.svg)](https://github.com/mmark-md/mmark/actions/workflows/ci.yaml)

* [Quick start: MMark vs GitHub-flavored markdown](#quick-start-mmark-vs-github-flavored-markdown)
* [MMark and CommonMark](#mmark-and-commonmark)
    * [Differences in inline parsing](#differences-in-inline-parsing)
    * [Other differences](#other-differences)
* [About MMark-specific extensions](#about-mmark-specific-extensions)
* [Performance](#performance)
* [Related packages](#related-packages)
* [Contribution](#contribution)
* [License](#license)

MMark (read “em-mark”) is a strict markdown processor for writers. “Strict”
means that not every input is considered a valid markdown document and parse
errors are possible and even desirable, because they allow us to spot markup
issues without searching for them in the rendered document. If a markdown
document passes the MMark parser, then it is likely to produce HTML output
without quirks. This feature makes it a good choice for writers and
bloggers.

MMark features:

* A parser that produces high-quality error messages and does not choke on
  the first parse error. It is capable of reporting several parse errors
  simultaneously.

* An extension system that allows us to create extensions that alter a
  parsed markdown document or the way it is rendered. Extensions can perform
  effects and can report errors of their own, which are shown against the
  source of the document just like parse errors are.

* A [`lucid`](https://hackage.haskell.org/package/lucid)-based renderer.

## Quick start: MMark vs GitHub-flavored markdown

It's easy to start using MMark if you're used to GitHub-flavored markdown.
There are three main differences:

1. URIs are not automatically recognized; you must enclose them in `<` and
   `>`.

2. HTML blocks and inline HTML are not supported.

3. See [differences in inline parsing](#differences-in-inline-parsing).

## MMark and CommonMark

MMark mostly tries to follow the CommonMark specification as given here:

https://spec.commonmark.org/0.31.2/

However, due to the fact that we do not allow inputs that do not make sense,
and also try to guard against common mistakes (like writing `##My header`
and having it rendered as a paragraph starting with hashes), MMark obviously
can't follow the specification precisely. In particular, parsing of inlines
is stricter than CommonMark (see below).

Another difference between CommonMark and MMark is that the latter supports
more (pun alert) common markdown extensions out of the box. In particular,
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

Emphasis and strong emphasis is an especially hairy topic in the CommonMark
specification. There are 17 ad-hoc rules defining the interaction between
`*` and `_` -based emphasis and more than half of all CommonMark
examples (that's about 300) test just this.

Almost none of that complexity is in deciding *what a delimiter run could
do*—CommonMark's notion of left- and right-flanking delimiter runs is
straightforward. It is in deciding what to do with a run that could just as
well open emphasis as close it, and the answer to that is a pile of special
cases that is hard to implement and harder for a human to remember.

MMark classifies delimiter runs exactly the way CommonMark does and then
resolves the ambiguous ones with a single rule. Let's start by dividing all
characters into four groups:

* **Space characters**, including space, tab, newline, carriage return, and
  other characters like non-breaking space.

* **Markup characters**, including the following: `*`, `~`, `_`, `` ` ``,
  `^`, `[`, `]`. These are used for markup and whenever they appear in a
  document, they must form valid markup constructions. To be used as
  ordinary punctuation characters they must be backslash escaped (there is
  exactly one exception to this, see below).

* **Punctuation characters**, which include all punctuation characters that
  are not **markup characters**. Following CommonMark, symbols such as `$`,
  `+`, and `=` count as punctuation here too.

* **Other characters**, which include all characters not falling into the
  three groups described above.

Next, let's assign *levels* to all groups but **markup characters**:

* **Space characters**—level 0
* **Punctuation characters**—level 1
* **Other characters**—level 2

When **markup characters** or **punctuation characters** are escaped with
backslash they become **other characters**.

Now take a run of **markup characters** placed between a character of level
`L` and a character of level `R`. It leans towards whichever of its two
neighbours is more solid, and that is what decides what it can do:

* `level(L) < level(R)`—the run hangs on the left hand side of a word, so it
  can only *open* emphasis markup (and other similar things like
  strikethrough, which we won't mention explicitly anymore for brevity);
* `level(L) > level(R)`—the run hangs on the right hand side of a word, so
  it can only *close* emphasis markup;
* `level(L) == level(R) == 0`—there is white space on both sides of the run,
  so it can do neither and the run is a parse error;
* `level(L) == level(R) > 0`—the run leans nowhere, so it is *ambiguous*.

The first two cases are exactly what the CommonMark specification calls a
left-flanking delimiter run that is not right-flanking, and a right-flanking
delimiter run that is not left-flanking. The last case is a run that is
both, and it is the only one where MMark has to make a decision of its own:

> An ambiguous run closes the markup it is inside of and opens new markup
> otherwise.

That is the whole rule, and it is what makes emphasis on a part of a word
work:

```
un*frigging*believable
H~2~O is not O~2~
x^2^ + y^2^ = z^2^
```

There is one exception to all of the above, and it is about the `_`
character. A run of underscores that has word characters on both sides of it
is not markup at all, it is literal text:

```
snake_case and to_string() and __dunder__
```

This is the one place where a **markup character** does not have to be
backslash escaped to be taken literally, and it exists because underscores
are so common inside identifiers. Asterisks are the way to emphasize a part
of a word.

A run with white space on both sides of it leans nowhere and can do nothing,
so these do not parse:

```
*Something * is not right.
Something __is __ not right.
```

Neither does a run that closes markup that was never opened:

```
Here goes bar*
```

Nor markup that is opened and never closed. That last one is what makes
`__foo__bar` an error rather than literal text: the first `__` opens strong
emphasis, the second one is inside a word and so is literal, and nothing
closes the strong emphasis afterwards.

### Other differences

Block-level parsing:

* If a line starts with hash signs it is expected to be a valid *non-empty*
  header (level 1–6 inclusive). If you want to start a paragraph with
  hashes, just escape the first hash with backslash and that will be enough.
* Setext headings are not supported for the sake of simplicity.
* Fenced code blocks must be explicitly closed by a closing fence. They are
  not closed by the end of document or by start of another block.
* Lists are defined by column at which their content starts. Content
  belonging to a particular list should start at the same column (or greater
  column, up to the column where indented code blocks start). As a
  consequence of this, lists do not feature “laziness”, unlike in
  CommonMark.
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
  reference is not enclosed with `<` and `>`, then the closing parenthesis
  character `)` is not considered part of the URI (use `<uri>` syntax if you
  want a closing parenthesis as part of a URI). Since the empty string is a
  valid URI and it may be confusing in some cases, we also force the user to
  write `<>` to represent the empty URI.
* Putting links in the text of another link is not allowed, i.e. no nested
  links are possible.
* Putting images in the description of other images is not allowed (similarly
  to the situation with links).
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

Distributed under the BSD 3-clause license.
