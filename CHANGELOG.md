## Unpublished

* Transformations can now report errors. A transformation runs in the new
  `TransT` monad and can `report` an error at a `Span` and carry on, or
  `abort` and give up on the document. Errors are collected in a
  `ParseErrorBundle Text TransError`, the same type the parser produces, so
  `errorBundlePretty` renders them against the source of the document
  exactly like parse errors.

* Extensions can now perform effects. `TransT` is a monad transformer, so a
  transformation may be run in `IO` or in any other monad, see `runTransM`.

* Every block and inline now carries the `Span` of the source it derives
  from, see `blockSpan` and `inlineSpan`. A node that an extension creates
  in place of another one inherits its `Span`, and a node assembled from
  several others should be given the `spanUnion` of theirs.

* `runScanner` and `runScannerM` take the document as their second argument
  now rather than their first, which is the order the rest of the pipeline
  already used and which lets a scanner be partially applied:
  `documentMetadata = runScanner metadataScanner`.

* Added `runCheck` and `runCheckM`, which run a computation in the
  transformation monad once against a document instead of applying it to
  every top-level block. A check that concerns the document as a whole no
  longer has to be written as a transformation of a block it has no
  interest in.

* Transformations are now applied to the document right away with `runTrans`
  and `runTransM`, instead of being accumulated in an extension value and
  applied just before rendering. `useExtension`, `useExtensions`,
  `blockTrans`, and `inlineTrans` are gone, and so is the `Endo`-based
  ordering that came with them: transformations are sequenced with `(>=>)`
  and abort as soon as one of them reports an error.

* Transformations are explicit and available in both directions:
  `bottomUpBlocks`, `topDownBlocks`, `bottomUpInlines`, and
  `topDownInlines`. The function given to `runTransM` is applied to
  top-level blocks only, so the transformation that reaches the rest of the
  document is the caller's choice.

* Rendering extensions cannot fail. They are collected in a
  `RenderExtension` value, which is now passed to `render` explicitly rather
  than being stored in the document: `render :: RenderExtension -> MMark ->
  Html ()`. Use `mempty` when there are none. Anything that can fail belongs
  in a transformation.

* Every constructor of `Block` and `Inline` now takes the `Span` of the
  source it derives from as its first argument.

* The `Text.MMark.Extension` module is gone. The two kinds of extension now
  have a module each: `Text.MMark.Trans` for transformations and
  `Text.MMark.Render` for render extensions. Both re-export the document
  types, so writing either kind of extension takes one import. `scanner` and
  `scannerM` moved to `Text.MMark`, next to `runScanner` and `runScannerM`.

* Block quotes now follow the CommonMark specification. Every line of a
  block quote must begin with a `>` character, one per level of nesting,
  instead of the quote continuing for as long as its content is indented.
  Paragraphs inside a block quote can be continued lazily, that is, on lines
  that lack the character. Note that fenced code blocks still have to be
  closed explicitly, so a code fence that is opened inside a block quote and
  not closed before the quote ends is a parse error.

* Block quotes now take precedence over tables. A line that begins with a
  `>` character opens a block quote even when it looks like the header of a
  table, so `> foo | bar` is a table inside a block quote instead of a table
  whose first header cell is `> foo`. Unlike paragraphs, tables cannot be
  continued lazily: a row that does not carry the block quote markers of the
  table it belongs to ends both the table and the quote.

* Emphasis, strong emphasis, strikeout, subscript, and superscript can now
  be applied to a part of a word. A delimiter run that could both open or
  close markup used to be rejected; it is now taken to close the markup it
  is inside of and to open new markup otherwise. Delimiter runs that lean
  unambiguously one way or the other are classified exactly as before.

* A delimiter run now opens all of its markup as one group, however long the
  run is, instead of being split into nested groups of at most two frames
  each. The delimiters of a run consequently close from the inside out at
  any length, which only changes the result for runs of five characters and
  more: `_____foo_____` is now `<em><strong><strong>foo</strong></strong></em>`
  as in CommonMark, rather than `<strong><strong><em>foo</em></strong></strong>`.

* A run of underscores surrounded by word characters is now literal text
  rather than markup, so `snake_case` and `to_string()` no longer have to be
  escaped. This is the only case in which a markup character does not have
  to be backslash escaped to be taken literally.

* Added the `UnmatchedClosingDelimiterRun` constructor to `MMarkErr`. A
  delimiter run that can only close markup but has no markup to close used
  to be reported as `NonFlankingDelimiterRun`; the latter is now reserved
  for runs that have white space on both sides of them and so can neither
  open nor close anything. Both errors are also reported at the beginning of
  the whole delimiter run now, rather than at the beginning of the part of
  it that MMark happened to recognize.

* An unclosed code fence whose last line lacks a line ending is now reported
  as “expecting closing code fence or code block content” rather than as
  “expecting newline”.

* The contents of a code span are no longer normalized by collapsing every
  run of white space into a single space and trimming both ends. Following
  CommonMark, only line endings become spaces now, and a single space is
  removed from each end when the contents both begin and end with a space
  without consisting of spaces alone. White space inside a code span is
  therefore preserved verbatim, so `` `col1  col2` `` keeps its two spaces
  and `` `a<tab>b` `` keeps its tab.

* Fixed a bug that made the info string of a fenced code block reject
  backtick characters even when the fence was made of tildes. Only a
  backtick fence can be confused with a backtick in its info string, so
  ` ~~~ aa ``` ~~~ ` opens a code block now instead of being a parse error.

* Symbols such as `$`, `+`, and `=` now count as punctuation when the type
  of the characters around a delimiter run is determined, as they do in
  CommonMark since version 0.31. Emphasis cannot hang on such a character
  anymore, so `*$*alpha` is a parse error rather than emphasized `$`.

* The test suite now follows the CommonMark specification 0.31.2 rather than
  0.28.

## MMark 0.0.8.0

* Exposed the following modules: `Text.MMark.Internal.Type`,
  `Text.MMark.Render`, `Text.MMark.Trans`, `Text.MMark.Util`.

## MMark 0.0.7.6

* The test suite now passes with `modern-uri-0.3.4.4`.

## MMark 0.0.7.5

* The test suite now passes with `modern-uri-0.3.4.3`.

## MMark 0.0.7.4

* The test suite has been fixed again and for good.

## MMark 0.0.7.3

* The test suite passes with `modern-uri-0.3.4` and later.

* Dropped support for GHC 8.6.x and older. Added support for GHC 9.0.1.

## MMark 0.0.7.2

* Uses Megaparsec 8.0.0.

* Dropped suppot for GHC 8.2.

## MMark 0.0.7.1

* Builds with `yaml-0.11.1.0`.

* Dropped support for GHC 8.0 and older.

## MMark 0.0.7.0

* Added GHCJS support by making `yaml` dependency optional. With GHCJS a
  yaml block simply always returns the empty object.

## MMark 0.0.6.2

* Fixed setting offset after parsing of collapsed reference links.
  Previously offset in parser state was restored incorrectly and errors that
  would happen after such links would be reported two characters before
  their real position.

## MMark 0.0.6.1

* Dropped `data-default-class` dependency.

## MMark 0.0.6.0

* Uses Megaparsec 7. The `parse` function now returns `ParseErrorBundle` on
  failure.

* Dropped `parseErrorsPretty`, use `errorBundlePretty` from `megaparsec`
  instead.

## MMark 0.0.5.7

* Improved parse errors related to the optional YAML block.

## MMark 0.0.5.6

* Now `blockTrans` and `inlineTrans` are applied to deeply nested elements
  too, not only top-level elements.

## MMark 0.0.5.5

* Fixed the bug in parser which signalled a parse error when YAML block was
  followed by more than one newline without markdown content after it.

## MMark 0.0.5.4

* Empty autolinks are now disallowed. `<>` will result in literal `<>` in
  resulting HTML.

## MMark 0.0.5.3

* Now HTML is escaped properly inside inline code spans.

## MMark 0.0.5.2

* Fixed the bug that prevented application of rendering extensions to
  sub-blocks (blocks contained inside other blocks) and sub-inlines (inlines
  contained inside other inlines).

## MMark 0.0.5.1

* The parser can now recover from block-level parse errors in tables and
  continue parsing.

* Pipes in code spans in table cells are not considered as table cell
  delimiters anymore.

* Table sub-parser now faster rejects inputs that do not look like a table,
  this improves overall performance.

* Better handling of the cases when a block can be interpreted as a list and
  as a table at the same time.

## MMark 0.0.5.0

* Documentation improvements.

* Added a dummy `Show` instance for the `MMark` type.

## MMark 0.0.4.3

* Compiles with `modern-uri-0.2.0.0` and later.

## MMark 0.0.4.2

* Made parsing of emphasis-like markup more flexible and forgiving, see
  `README.md` for more information.

## MMark 0.0.4.1

* This version uses `megaparsec-6.4.0` and `parser-combinators-0.4.0` and
  has improved performance.

## MMark 0.0.4.0

* Added support for pipe tables (like on GitHub).

* Fixed a nasty space leak in the parser, made it faster too.

## MMark 0.0.3.2

* Empty strings are not parsed as URIs anymore (even though a valid URI may
  be represented as the empty string). Instead, it's now possible to write
  an empty URI using the `<>` syntax (which previously was not recognized as
  a URI in some contexts).

* Improved parse errors related to parsing of titles in links, images, and
  reference definitions.

* Parsing of reference definitions now can recover from failures, so the
  parser doesn't choke on malformed reference definitions anymore.

* Reduced allocations and improved speed of the parser significantly.

## MMark 0.0.3.1

* Fixed a couple of bugs in the parser for reference definitions.

* Now link and image titles may contain newline character as per the Common
  Mark spec.

## MMark 0.0.3.0

* Code can interrupt paragraphs now, as per CommonMark spec.

* Implemented parsing of reference-links (including collapsed and
  shortcut-style links).

* Implemented parsing of reference-style images (including collapsed and
  shortcut-style images).

* Added support for entity and numeric references (section 6.2 of the Common
  Mark spec).

* Improved quality of parse errors.

## MMark 0.0.2.1

* Improved performance of the parser. Mainly the inline-level parser to be
  precise. The result is that now there are 3× less allocations and the code
  runs about 3× faster on paragraphs and block quotes (it's about 2.5×
  faster for a big realistic document).

* Improved quality of parse errors.

## MMark 0.0.2.0

* Now punctuation is stripped from header ids in
  `Text.MMark.Extension.headerId`.

* Added `scannerM` in `Text.MMark.Extension` and `runScannerM` in
  `Text.MMark`.

* Added support for block quotes.

* Added support for unordered and ordered lists.

## MMark 0.0.1.1

* Fixed a bug in skipping of headers (only one newline after the header line
  was picked, not all white space up to next block).

## MMark 0.0.1.0

* Initial release.
