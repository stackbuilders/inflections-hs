# String Inflections for Haskell

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)
[![Hackage](https://img.shields.io/hackage/v/inflections.svg)](http://hackage.haskell.org/package/inflections)
[![Stackage Nightly](http://stackage.org/package/inflections/badge/nightly)](http://stackage.org/nightly/package/inflections)
[![Stackage LTS](http://stackage.org/package/inflections/badge/lts)](http://stackage.org/lts/package/inflections)
[![Build](https://github.com/stackbuilders/inflections-hs/actions/workflows/build.yml/badge.svg)](https://github.com/stackbuilders/inflections-hs/actions/workflows/build.yml)

<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-1-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->

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

## Future work

Ideally, we want to implement other functions found in the Rails
inflections library. If you need one of those functions, please submit a pull request!

## Further documentation

For more information, please see the the
[Haddock docs for this module](http://hackage.haskell.org/package/inflections/docs/Text-Inflections.html).

## Author

Justin Leitgeb <justin@stackbuilders.com>

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://cristhianmotoche.github.io/"><img src="https://avatars.githubusercontent.com/u/8370088?v=4?s=100" width="100px;" alt="Cristhian Motoche"/><br /><sub><b>Cristhian Motoche</b></sub></a><br /><a href="https://github.com/stackbuilders/inflections-hs/commits?author=CristhianMotoche" title="Code">💻</a></td>
    </tr>
  </tbody>
  <tfoot>
    <tr>
      <td align="center" size="13px" colspan="7">
        <img src="https://raw.githubusercontent.com/all-contributors/all-contributors-cli/1b8533af435da9854653492b1327a23a4dbd0a10/assets/logo-small.svg">
          <a href="https://all-contributors.js.org/docs/en/bot/usage">Add your contributions</a>
        </img>
      </td>
    </tr>
  </tfoot>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome!

## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](/docs/CONTRIBUTING.md) to know how you can help us build it.

---
<img src="https://cdn.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)
