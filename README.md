# MMark

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/mmark.svg?style=flat)](https://hackage.haskell.org/package/mmark)
[![Stackage Nightly](http://stackage.org/package/mmark/badge/nightly)](http://stackage.org/nightly/package/mmark)
[![Stackage LTS](http://stackage.org/package/mmark/badge/lts)](http://stackage.org/lts/package/mmark)
[![Build Status](https://travis-ci.org/mrkkrp/mmark.svg?branch=master)](https://travis-ci.org/mrkkrp/mmark)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/mmark/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/mmark?branch=master)

MMark (read “em-mark”) is a strict markdown processor for writers. “Strict”
means that not every input is considered valid markdown and parse errors are
possible and even desirable, so one can know that something is not quite
right before looking at incorrect (or rather simply unexpected) result of
rendering. This feature makes it a good choice for writers and bloggers.

## MMark and Common Mark

MMark tries to follow the Common Mark specification as given here:

<https://github.com/jgm/CommonMark>

However, due to the fact that we do not allow inputs that do not make sense,
MMark obviously can't follow the specification precisely. In particular,
parsing of inlines differs considerably from Common Mark. For example, some
characters like `*`, `_`, etc. that must appear in markup in what is called
left- and right-flanking delimiter runs are allowed only in those positions:

```
*Something* is not right.
Something __is__ not right.
```

This produces a parse error:

```
*Something * is not right.
Something __is __ not right.
```

Here is the full list of so-called **markup characters**: `*`, `~`, `_`, ``
` ``, `^`, `[`, `]`. When they appear without escaping, they must form
correct markup structures, otherwise parse errors will be reported.

The same applies to the syntax of links, images, etc. For example, it's a
parse error to put a link into text of another link.

Another difference between Common Mark and MMark is that the latter supports
more common markdown extensions out-of-the-box. In particular, MMark
supports:

* parsing of optional YAML metadata block (NOT YET)
* automatic turning of bare URIs into links (NOT YET)
* strikeout using `~~this~~` syntax
* superscript using `^this^` syntax
* subscript using `~this~` syntax
* PHP-style footnotes, e.g. `[^1]` (NOT YET)
* “pipe” tables (as used on GitHub) (NOT YET)

You do not need to enable or tweak anything for these to work, they are
built-in features.

## Features to implement before 1.0.0.0

Up-to-date list of features we need to add (in order of importance):

* inline images
* inline autolinks
* hard line breaks
* setext headings
* HTML blocks
* HTML inline blocks
* link reference definitions (also adjust link parser so references work)
* blockquotes
* unordered lists
* ordered lists
* parsing of optional YAML metadata block
* automatic turning of bare URIs into links
* PHP-style footnotes, e.g. `[^1]`
* “pipe” table (as used on GitHub)
* entity and numeric character references
* decide on URL manipulation (should we correct “bad” characters using
  URL-encoding automatically?)

Other:

* benchmarking and optimization.

## Contribution

Issues, bugs, and questions may be reported in
[the GitHub issue tracker for this project](https://github.com/mrkkrp/mmark/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2017 Mark Karpov

Distributed under BSD 3 clause license.
