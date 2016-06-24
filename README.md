# String Inflections for Haskell

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)
[![Hackage](https://img.shields.io/hackage/v/inflections.svg)](http://hackage.haskell.org/package/inflections)
[![Stackage Nightly](http://stackage.org/package/inflections/badge/nightly)](http://stackage.org/nightly/package/inflections)
[![Stackage LTS](http://stackage.org/package/inflections/badge/lts)](http://stackage.org/lts/package/inflections)
[![Build Status](https://travis-ci.org/stackbuilders/inflections-hs.svg?branch=master)](https://travis-ci.org/stackbuilders/inflections-hs)

This library is a partial port of the
[String Inflector](http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html)
from Ruby on Rails. It currently implements methods such as `parameterize`,
`transliterate`, `camelize`, `underscore` and `dasherize`. Please see the
haddock documentation for a complete list of the functions implemented by this
library.

Unlike the ActiveSupport (Rails) and Ember implementations of inflections, this
library uses a parser to verify the input to functions like `camelize`. This is
done to ensure that the output strings adhere to the syntax that they are
supposed to generate. You can read more about the philosophy behind this library
in the
[Haddock documentation](http://hackage.haskell.org/package/inflections/docs/Text-Inflections.html).

## Usage

The following examples demonstrate usage of the `parameterize`, `transliterate`
and `camelize` functions:

```haskell
λ: parameterize "Hola. ¿Cómo estás?"
"hola-como-estas"

λ: transliterate "Hola. ¿Cómo estás?"
"Hola. ?Como estas?"

λ: import Control.Monad (liftM)
λ: liftM camelize $ parseSnakeCase "hey_there"
"HeyThere"
```

## Customization

Part of parameterizing strings is approximating all characters in the input
encoding to ASCII characters. This library copies the character approximation
table from the Ruby i18n library. This data structure is provided as
`defaultCharacterTransliterations`. You can provide your own transliteration map
by passing a Map structure (from Data.Map) to the `parameterizeCustom` function.

If you want to specify a custom default replacement or approximation table for
the `transliterate` function, you should instead call the `transliterateCustom`
function which accepts a String for replacements and a Map for substitution.

## TODO

I'd like this library to implement other functions found in the Rails
inflections library. If you need one of those functions, please submit a pull
request!

## Further documentation

For more information, please see the the
[Haddock docs for this module](http://hackage.haskell.org/package/inflections/docs/Text-Inflections.html).

## Author

Justin Leitgeb <justin@stackbuilders.com>

## Contributing

You may submit pull requests to this repository on GitHub. Tests are appreciated with your contribution.

## License

MIT - see [the LICENSE file](LICENSE).
