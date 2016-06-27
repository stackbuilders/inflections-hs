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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text

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

-- type Countables = Map Regex Text

-- https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb
-- countableMap :: Countables
-- countableMap = M.fromList []
  -- plural
  -- (/$/, 's')
  -- (/s$/i, 's')
  -- (/^(ax|test)is$/i, '\1es')
  -- (/(octop|vir)us$/i, '\1i')
  -- (/(octop|vir)i$/i, '\1i')
  -- (/(alias|status)$/i, '\1es')
  -- (/(bu)s$/i, '\1ses')
  -- (/(buffal|tomat)o$/i, '\1oes')
  -- (/([ti])um$/i, '\1a')
  -- (/([ti])a$/i, '\1a')
  -- (/sis$/i, 'ses')
  -- (/(?:([^f])fe|([lr])f)$/i, '\1\2ves')
  -- (/(hive)$/i, '\1s')
  -- (/([^aeiouy]|qu)y$/i, '\1ies')
  -- (/(x|ch|ss|sh)$/i, '\1es')
  -- (/(matr|vert|ind)(?:ix|ex)$/i, '\1ices')
  -- (/^(m|l)ouse$/i, '\1ice')
  -- (/^(m|l)ice$/i, '\1ice')
  -- (/^(ox)$/i, '\1en')
  -- (/^(oxen)$/i, '\1')
  -- (/(quiz)$/i, '\1zes')

  -- singular
  -- (/s$/i, '')
  -- (/(ss)$/i, '\1')
  -- (/(n)ews$/i, '\1ews')
  -- (/([ti])a$/i, '\1um')
  -- (/((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$/i, '\1sis')
  -- (/(^analy)(sis|ses)$/i, '\1sis')
  -- (/([^f])ves$/i, '\1fe')
  -- (/(hive)s$/i, '\1')
  -- (/(tive)s$/i, '\1')
  -- (/([lr])ves$/i, '\1f')
  -- (/([^aeiouy]|qu)ies$/i, '\1y')
  -- (/(s)eries$/i, '\1eries')
  -- (/(m)ovies$/i, '\1ovie')
  -- (/(x|ch|ss|sh)es$/i, '\1')
  -- (/^(m|l)ice$/i, '\1ouse')
  -- (/(bus)(es)?$/i, '\1')
  -- (/(o)es$/i, '\1')
  -- (/(shoe)s$/i, '\1')
  -- (/(cris|test)(is|es)$/i, '\1is')
  -- (/^(a)x[ie]s$/i, '\1xis')
  -- (/(octop|vir)(us|i)$/i, '\1us')
  -- (/(alias|status)(es)?$/i, '\1')
  -- (/^(ox)en/i, '\1')
  -- (/(vert|ind)ices$/i, '\1ex')
  -- (/(matr)ices$/i, '\1ix')
  -- (/(quiz)zes$/i, '\1')
  -- (/(database)s$/i, '\1')

simpleCountableMap :: Bimap.Bimap Text Text
simpleCountableMap = Bimap.fromList [
  -- irregular
  -- from singular to plural
    ("person", "people")
  , ("man", "men")
  , ("child", "children")
  , ("sex", "sexes")
  , ("move", "moves")
  , ("zombie", "zombies")

  -- uncountable
  , ("equipment", "equipment")
  , ("information", "information")
  , ("rice", "rice")
  , ("money", "money")
  , ("species", "species")
  , ("series", "series")
  , ("fish", "fish")
  , ("sheep", "sheep")
  , ("jeans", "jeans")
  , ("police", "police")
  ]

