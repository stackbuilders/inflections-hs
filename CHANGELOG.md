## Inflections 0.3.0.0

* Added the `CHANGELOG.md` file.

* Switched test suite to Hspec.

* Switched to Megaparsec 5 for parsing.

* Made the API use `Text` instead of `String` (which significally improved
  speed).

* The `toUnderscore`, `toDashed`, and `toCamelCased` are not partial
  anymore, now they operate in `MonadThrow`.

* Improved documentation.

## Inflections 0.2.0.1

* Support for GHC 8.0.

## Inflections 0.2.0.0

* Added `other-modules` to test suite.

## Inflections 0.1.0.10

* Support for GHC 7.10.

## Inflections 0.1.0.9

* Support for GHC 7.8.

## Inflections 0.1.0.8

* Fixed a typo in docs of `humanize`.

* Added `toUnderscore`, `toDashed`, and `toCamelCased`.

## Inflections 0.1.0.7

* Support for `base-4.7`.

* Improved documentation.

## Inflections 0.1.0.6

* Added `titleize` and `humanize`.

* Improved documentation.

## Inflections 0.1.0.5

* Added module documentation for `Text.Inflections`.

## Inflections 0.1.0.4

* Reduced number of public modules to one: `Text.Inflections`.

* Added `ordinal` and `ordinalize`.

* Improved documentation.

## Inflections 0.1.0.3

* Added `camelize`, `camelizeCustom`, `underscore`, and `underscoreCustom`.

* Made the word parser accept empty input.

* Improved documentation.

## Inflections 0.1.0.2

* Added the `transliterate` and `transliterateCustom` functions.

## Inflections 0.1.0.1

* No changes.

## Inflections 0.1.0.0

* Initial release.
