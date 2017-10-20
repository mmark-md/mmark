# MMark

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/mmark.svg?style=flat)](https://hackage.haskell.org/package/mmark)
[![Stackage Nightly](http://stackage.org/package/mmark/badge/nightly)](http://stackage.org/nightly/package/mmark)
[![Stackage LTS](http://stackage.org/package/mmark/badge/lts)](http://stackage.org/lts/package/mmark)
[![Build Status](https://travis-ci.org/mrkkrp/mmark.svg?branch=master)](https://travis-ci.org/mrkkrp/mmark)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/mmark/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/mmark?branch=master)

MMark (read “em-mark”) is a strict markdown processor for writers. “Strict”
means that not every input is considered valid markdown and parse errors are
possible and even desirable because they allow to spot markup issues without
searching for them in rendered document. If a markdown document passes MMark
parser, then it'll most certainly produce HTML without any unexpected
artifacts. This feature makes it a good choice for writers and bloggers.

## MMark and Common Mark

MMark tries to follow the Common Mark specification as given here:

https://github.com/jgm/CommonMark

However, due to the fact that we do not allow inputs that do not make sense,
MMark obviously can't follow the specification precisely. In particular,
parsing of inlines differs considerably from Common Mark.

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

### Differences in inline parsing

Emphasis and strong emphasis is an especially hairy topic in Common Mark.
There are 17 ad-hoc rules definition interaction between `*` and `_` -based
emphasis and more than half of all Common Mark examples (that's about 300)
test this tricky logic.

Not only it is hard to implement (for tools built around markdown too), it's
hard to understand for humans too. For example, this input:

```
*(*foo*)*
```

produces this HTML:

```
<p><em>(<em>foo</em>)</em></p>
```

(Note the nested emphasis.)

Could it produce something like this instead?

```
<p><em>(</em>foo<em>)</em></p>
```

Well, it could! Without remembering those 17 ad-hoc rules, there going to be
a lot of tricky cases when a user won't be able to tell how markdown will be
parsed.

I decided to make parsing of emphasis, strong emphasis, and similar
constructs like strikethrough, subscript, and superscript more symmetric and
less ad-hoc. This is a work in progress and I'm not fully satisfied with the
current approach, and it does not allow to express some combinations of
characters and markup, but in 99% of practical cases it works just like
Common Mark and normal markdown intuitions will work OK for the users.

Let's start by dividing all characters into three groups:

* **Markup characters**, including the following: `*`, `~`, `_`, `` ` ``,
  `^`, `[`, `]`. These are used for markup and whenever they appear in a
  document, they must form valid markup constructions.

* **Space characters**, including space, tab, newline, carriage return, and
  some Unicode space characters.

* **Other characters**, which include all characters not falling into the
  two groups described above.

**Markup characters** and even **space characters** can be “converted” to
**other characters** via backslash escaping. We'll see how this is useful in
a few moments.

We'll call **markdown characters** placed between **space characters** and
**other characters** *left-flanking delimiter run*. These markup characters
sort of hang on the left hand side of a word.

Similarly we'll call **markdown characters** placed between **other
characters** and **space characters** *right-flanking delimiter run*. These
hang on the right hand side of a word.

Emphasis markup (and other similar things like strikethrough, which we won't
mention explicitly anymore for brevity) can start only as left-flanking
delimiter run and end only as right-flanking delimiter run.

This produces a parse error:

```
*Something * is not right.
Something __is __ not right.
```

And this too:

```
__foo__bar
```

This means that inter-word emphasis is not supported by this approach.

There is one more tricky thing. In some cases we want to end emphasis and
have full stop or other punctuation right after it:

```
Here it *goes*.
```

You can see that the closing `*` is not in right-flanking position here, and
so it's a parse error. To avoid this, some punctuation that normally appears
outside of markup were made “transparent” and it's regarded as white space,
so the example above parses correctly and works as expected. To put a
transparent character inside emphasis, backslash escaping is necessary:

```
We *\(can\)* have it.
```

Here `(` and `)` are transparent punctuation characters, just like `.`, so
they must be turned into **other characters** to go inside the emphasis.
This is a corner case and should not be common in practice.

So far the main limitation of this approach is pains with inter-word markup,
as in this example:

```
**We started to work on the *issue*.**
```

Should we escape `.` here? On one hand we should, to close `**`. But if we
do, the closing `*` won't be in right-flanking position anymore. God damn
it.

### Other differences

Other differences/incompatibilities with Common Mark specification include
(the list will hopefully get shorter as the library matures):

* Fenced code blocks must be explicitly closed by a closing fence. They are
  not closed by the end of document or by start of another block.
* MMark does not support hard line breaks represented as double space before
  newline. Nevertheless, hard line breaks in the form of backslash before
  newline are supported.
* Link destination cannot contain unescaped parentheses even if they form a
  balanced pair. If you want them there, escape them like other punctuation
  characters.
* Nesting images into description of other images is not allowed (similarly
  to the situation with links).
* Empty image descriptions are not allowed.
* Separate declaration of image's source and title is not (yet) supported.
* Inline autolinks are not supported yet.
* Blockquotes are not supported yet.
* Lists (unordered and ordered) are not supported yet.
* Setext headings are not supported yet.
* Reference links are not supported yet.
* HTML blocks are not supported yet.
* HTML inlines are not supported yet.
* Entity and numeric character references are not supported yet.

## Contribution

Issues, bugs, and questions may be reported in
[the GitHub issue tracker for this project](https://github.com/mrkkrp/mmark/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2017 Mark Karpov

Distributed under BSD 3 clause license.
