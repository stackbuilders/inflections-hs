# String Inflections for Haskell

This library is a partial port of the [String Inflector](http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html) from Ruby on Rails. Specifically, it implements the [`parameterize`](http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html#method-i-parameterize) and [`dasherize`](http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html#method-i-dasherize) functions from the Inflector.

## Usage

The most common usage of this library at this point is to parameterize a URL. This is accomplished as follows:

```haskell
λ: parameterize defaultTransliterations "¡Feliz año nuevo!"
"feliz-ano-nuevo"
```

## Customization

Part of parameterizing strings is approximating all characters in the input encoding to ASCII characters. This library copies the character approximation table from the Ruby i18n library. This data structure is provided as `defaultCharacterTransliterations`. You can provide your own transliteration map by passing a Map structure (from Data.Map) to the `parameterize` function.

## TODO

I'd like this library to implement other functions found in the Rails inflections library. If you need one of those functions, please submit a pull request!

## Author

Justin Leitgeb <justin@stackbuilders.com>

## Contributing

You may submit pull requests to this repository on GitHub. Please add property
tests for any functional changes that you make to this library.

## License

MIT - see [the LICENSE file](LICENSE).
