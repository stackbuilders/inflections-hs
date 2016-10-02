{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Text.Inflections.Data
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Auxiliary data used in the library.

module Text.Inflections.Data where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Text (Text)

-- |A 'Data.Map.Map' containing mappings from international characters to
-- sequences approximating these characters within the ASCII range.
type Transliterations = HashMap Char String

-- |These default transliterations stolen from the Ruby i18n library - see
-- <https://github.com/svenfuchs/i18n/blob/master/lib/i18n/backend/transliterator.rb#L41:L69>.
defaultMap :: Transliterations
defaultMap = M.fromList [
  ('À', "A"), ('Á', "A"), ('Â', "A"), ('Ã', "A"), ('Ä', "A"), ('Å', "A"),
  ('Æ', "AE"), ('Ç', "C"), ('È', "E"), ('É', "E"), ('Ê', "E"), ('Ë', "E"),
  ('Ì', "I"), ('Í', "I"), ('Î', "I"), ('Ï', "I"), ('Ð', "D"), ('Ñ', "N"),
  ('Ò', "O"), ('Ó', "O"), ('Ô', "O"), ('Õ', "O"), ('Ö', "O"), ('×', "x"),
  ('Ø', "O"), ('Ù', "U"), ('Ú', "U"), ('Û', "U"), ('Ü', "U"), ('Ý', "Y"),
  ('Þ', "Th"), ('ß', "ss"), ('à', "a"), ('á', "a"), ('â', "a"), ('ã', "a"),
  ('ä', "a"), ('å', "a"), ('æ', "ae"), ('ç', "c"), ('è', "e"), ('é', "e"),
  ('ê', "e"), ('ë', "e"), ('ì', "i"), ('í', "i"), ('î', "i"), ('ï', "i"),
  ('ð', "d"), ('ñ', "n"), ('ò', "o"), ('ó', "o"), ('ô', "o"), ('õ', "o"),
  ('ö', "o"), ('ø', "o"), ('ù', "u"), ('ú', "u"), ('û', "u"), ('ü', "u"),
  ('ý', "y"), ('þ', "th"), ('ÿ', "y"), ('Ā', "A"), ('ā', "a"), ('Ă', "A"),
  ('ă', "a"), ('Ą', "A"), ('ą', "a"), ('Ć', "C"), ('ć', "c"), ('Ĉ', "C"),
  ('ĉ', "c"), ('Ċ', "C"), ('ċ', "c"), ('Č', "C"), ('č', "c"), ('Ď', "D"),
  ('ď', "d"), ('Đ', "D"), ('đ', "d"), ('Ē', "E"), ('ē', "e"), ('Ĕ', "E"),
  ('ĕ', "e"), ('Ė', "E"), ('ė', "e"), ('Ę', "E"), ('ę', "e"), ('Ě', "E"),
  ('ě', "e"), ('Ĝ', "G"), ('ĝ', "g"), ('Ğ', "G"), ('ğ', "g"), ('Ġ', "G"),
  ('ġ', "g"), ('Ģ', "G"), ('ģ', "g"), ('Ĥ', "H"), ('ĥ', "h"), ('Ħ', "H"),
  ('ħ', "h"), ('Ĩ', "I"), ('ĩ', "i"), ('Ī', "I"), ('ī', "i"), ('Ĭ', "I"),
  ('ĭ', "i"), ('Į', "I"), ('į', "i"), ('İ', "I"), ('ı', "i"), ('Ĳ', "IJ"),
  ('ĳ', "ij"), ('Ĵ', "J"), ('ĵ', "j"), ('Ķ', "K"), ('ķ', "k"), ('ĸ', "k"),
  ('Ĺ', "L"), ('ĺ', "l"), ('Ļ', "L"), ('ļ', "l"), ('Ľ', "L"), ('ľ', "l"),
  ('Ŀ', "L"), ('ŀ', "l"), ('Ł', "L"), ('ł', "l"), ('Ń', "N"), ('ń', "n"),
  ('Ņ', "N"), ('ņ', "n"), ('Ň', "N"), ('ň', "n"), ('ŉ', "'n"), ('Ŋ', "NG"),
  ('ŋ', "ng"), ('Ō', "O"), ('ō', "o"), ('Ŏ', "O"), ('ŏ', "o"), ('Ő', "O"),
  ('ő', "o"), ('Œ', "OE"), ('œ', "oe"), ('Ŕ', "R"), ('ŕ', "r"), ('Ŗ', "R"),
  ('ŗ', "r"), ('Ř', "R"), ('ř', "r"), ('Ś', "S"), ('ś', "s"), ('Ŝ', "S"),
  ('ŝ', "s"), ('Ş', "S"), ('ş', "s"), ('Š', "S"), ('š', "s"), ('Ţ', "T"),
  ('ţ', "t"), ('Ť', "T"), ('ť', "t"), ('Ŧ', "T"), ('ŧ', "t"), ('Ũ', "U"),
  ('ũ', "u"), ('Ū', "U"), ('ū', "u"), ('Ŭ', "U"), ('ŭ', "u"), ('Ů', "U"),
  ('ů', "u"), ('Ű', "U"), ('ű', "u"), ('Ų', "U"), ('ų', "u"), ('Ŵ', "W"),
  ('ŵ', "w"), ('Ŷ', "Y"), ('ŷ', "y"), ('Ÿ', "Y"), ('Ź', "Z"), ('ź', "z"),
  ('Ż', "Z"), ('ż', "z"), ('Ž', "Z"), ('ž', "z")]


-- |These default inflections stolen from the Ruby inflection library - see
-- https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb
defaultPlurals' :: [(Text, Text)]
defaultPlurals' =
  [ ("$", "s")
  , ("s$", "s")
  , ("^(ax|test)is$", "\1es")
  , ("(octop|vir)us$", "\1i")
  , ("(octop|vir)i$", "\1i")
  , ("(alias|status)$", "\1es")
  , ("(bu)s$", "\1ses")
  , ("(buffal|tomat)o$", "\1oes")
  , ("([ti])um$", "\1a")
  , ("([ti])a$", "\1a")
  , ("sis$", "ses")
  , ("(?:([^f])fe|([lr])f)$", "\1\2ves")
  , ("(hive)$", "\1s")
  , ("([^aeiouy]|qu)y$", "\1ies")
  , ("(x|ch|ss|sh)$", "\1es")
  , ("(matr|vert|ind)(?:ix|ex)$", "\1ices")
  , ("^(m|l)ouse$", "\1ice")
  , ("^(m|l)ice$", "\1ice")
  , ("^(ox)$", "\1en")
  , ("^(oxen)$", "\1")
  , ("(quiz)$", "\1zes")
  ]

-- |These default inflections stolen from the Ruby inflection library - see
-- https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb
defaultSingulars' :: [(Text, Text)]
defaultSingulars' =
  [ ("s$", "")
  , ("(ss)$", "\1")
  , ("(n)ews$", "\1ews")
  , ("([ti])a$", "\1um")
  , ("((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$", "\1sis")
  , ("(^analy)(sis|ses)$", "\1sis")
  , ("([^f])ves$", "\1fe")
  , ("(hive)s$", "\1")
  , ("(tive)s$", "\1")
  , ("([lr])ves$", "\1f")
  , ("([^aeiouy]|qu)ies$", "\1y")
  , ("(s)eries$", "\1eries")
  , ("(m)ovies$", "\1ovie")
  , ("(x|ch|ss|sh)es$", "\1")
  , ("^(m|l)ice$", "\1ouse")
  , ("(bus)(es)?$", "\1")
  , ("(o)es$", "\1")
  , ("(shoe)s$", "\1")
  , ("(cris|test)(is|es)$", "\1is")
  , ("^(a)x[ie]s$", "\1xis")
  , ("(octop|vir)(us|i)$", "\1us")
  , ("(alias|status)(es)?$", "\1")
  , ("^(ox)en", "\1")
  , ("(vert|ind)ices$", "\1ex")
  , ("(matr)ices$", "\1ix")
  , ("(quiz)zes$", "\1")
  , ("(database)s$", "\1")
  ]

-- |These default irregular inflections stolen from the Ruby inflection library - see
-- https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb
defaultIrregulars' :: [(Text, Text)]
defaultIrregulars' =
  -- from singular to plural
  [ ("person", "people")
  , ("man", "men")
  , ("child", "children")
  , ("sex", "sexes")
  , ("move", "moves")
  , ("zombie", "zombies")
  ]

-- |These default uncountable inflections stolen from the Ruby inflection library - see
-- https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb
defaultUncountables' :: [Text]
defaultUncountables' =
  [ "equipment"
  , "information"
  , "rice"
  , "money"
  , "species"
  , "series"
  , "fish"
  , "sheep"
  , "jeans"
  , "police"
  ]
