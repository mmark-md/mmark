## MMark 0.0.3.0

* Code can interrupt paragraphs now, as per Common Mark spec.

* Implemented parsing of reference-links (including collapsed and
  shortcut-style links).

* Implemented parsing of reference-style images (including collapsed and
  shortcut-style images).

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
