# MMark

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/mmark.svg?style=flat)](https://hackage.haskell.org/package/mmark)
[![Stackage Nightly](http://stackage.org/package/mmark/badge/nightly)](http://stackage.org/nightly/package/mmark)
[![Stackage LTS](http://stackage.org/package/mmark/badge/lts)](http://stackage.org/lts/package/mmark)
[![Build Status](https://travis-ci.org/mrkkrp/mmark.svg?branch=master)](https://travis-ci.org/mrkkrp/mmark)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/mmark/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/mmark?branch=master)

A flexible markdown processor for writers and bloggers. (Proper description
is pending.)

## Relation to Common Mark

MMark implements Common Mark specification
as [given here](https://github.com/jgm/CommonMark). We have a test suite
called `common-mark-spec` that runs examples from the spec and ensures that
MMark produces correct results.

The `common-mark-spec.json` file has been obtained from the original spec by
running:

```
$ python test/spec_tests.py --dump-tests > common-mark-spec.json
```

We'll try to keep the spec in this repo up to date.

## Contribution

Issues, bugs, and questions may be reported
in
[the GitHub issue tracker for this project](https://github.com/mrkkrp/mmark/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2017 Mark Karpov

Distributed under BSD 3 clause license.
