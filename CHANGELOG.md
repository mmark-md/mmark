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

* Code can interrupt paragraphs now, as per Common Mark spec.

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
