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
right before looking at an incorrect (or rather simply unexpected) result of
rendering. This feature makes it a good choice for writers and bloggers.

## MMark and Common Mark

MMark is mostly compatible with the Common Mark specification as given here:
<https://github.com/jgm/CommonMark>. However, in some cases it rejects
(arguably) questionable input and reports parse errors. Here is the full
list of cases when MMark diverges from Common Mark:

* TODO

We have a test suite called `common-mark-spec` that runs examples from the
spec and ensures that MMark produces correct results.

The `common-mark-spec.json` file has been obtained from the original spec by
running:

```
$ python test/spec_tests.py --dump-tests > common-mark-spec.json
```

We'll try to keep the spec in this repo up to date.

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

## Features to implement before the first release

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

## Contribution

Issues, bugs, and questions may be reported in
[the GitHub issue tracker for this project](https://github.com/mrkkrp/mmark/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2017 Mark Karpov

Distributed under BSD 3 clause license.
