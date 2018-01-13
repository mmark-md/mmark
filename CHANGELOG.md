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
