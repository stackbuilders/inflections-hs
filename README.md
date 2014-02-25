# String Inflections for Haskell

[![Build Status](https://travis-ci.org/stackbuilders/inflections-hs.png)](https://travis-ci.org/stackbuilders/inflections-hs)

This library is a partial port of the [String Inflector](http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html) from Ruby on Rails. Specifically, it implements the [`parameterize`](http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html#method-i-parameterize) and [`dasherize`](http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html#method-i-dasherize) functions from the Inflector.

## Usage

The following examples demonstrate usage of the `parameterize` and `transliterate` functions:

```haskell
λ: parameterize "Hola. ¿Cómo estás?"
"hola-como-estas"

λ: transliterate "Hola. ¿Cómo estás?"
"Hola. ?Como estas?"
```

## Customization

Part of parameterizing strings is approximating all characters in the input encoding to ASCII characters. This library copies the character approximation table from the Ruby i18n library. This data structure is provided as `defaultCharacterTransliterations`. You can provide your own transliteration map by passing a Map structure (from Data.Map) to the `parameterizeCustom` function.

If you want to specify a custom default replacement or approximation table for the `transliterate` function, you should instead call the `transliterateCustom` function which accepts a String for replacements and a Map for substitution.

## TODO

I'd like this library to implement other functions found in the Rails inflections library. If you need one of those functions, please submit a pull request!

## Author

Justin Leitgeb <justin@stackbuilders.com>

## Contributing

You may submit pull requests to this repository on GitHub. Please add property
tests for any functional changes that you make to this library.

## License

MIT - see [the LICENSE file](LICENSE).
