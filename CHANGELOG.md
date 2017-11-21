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
