## Inflections 0.4.0.3
* Support `megaparsec` == 7.0*.
* **Drop support** for `GHC 7.8.4`.

## Inflections 0.4.0.3
* Support `exceptions` == 0.10.*

## Inflections 0.4.0.2
* Support `exceptions` == 0.9.*

## Inflections 0.4.0.1
* Support `megaparsec` >= 6.4.0

## Inflections 0.4.0.0

* Update megaparsec to version 6.

## Inflections 0.3.0.0

* A more type-safe API forbidding creation of invalid words.

* Made the API use `Text` instead of `String` (which significally improved
  speed).

* Switched to Megaparsec 5 for parsing.

* Renamed `defaultMap` to `defaultTransliterations`.

* Words now can contain digits (recognized by all parsers).

* `parseSnakeCase` now is not confused when a word happens to have prefix
  coinciding with an acronym. This is harder to fix for `parseCamelCase`
  because acronym may contain capital letters, so old behavior is preserved
  for `parseCamelCase` for now.

* `parseCamelCase` and `parseSnakeCase` take any instance of `Foldable` as a
  collection of acronyms, not just lists.

* Added the `CHANGELOG.md` file.

* Switched test suite to Hspec.

* The `toUnderscore`, `toDashed`, and `toCamelCased` are not partial
  anymore. They return parse error in `Left` just like parsing functions,
  but this result can be lifted into any instance of `MonadThrow` with
  `betterThrow` helper.

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
