module Text.Inflections
    ( dasherize

    , decamelize
    , decamelizeCustom

    , defaultMap

    , parameterize
    , parameterizeCustom

    , transliterate
    , transliterateCustom
    )
where

import Data.Char (isAscii)
import qualified Data.Map as Map

import Text.Inflections.Data (defaultMap)

import Text.Inflections.Parameterize ( Transliterations
                                     , parameterize
                                     , parameterizeCustom )

import Text.Inflections.Decamelize ( decamelize
                                    , decamelizeCustom )

-- |Replaces underscores with dashes in the string.
dasherize :: String -> String
dasherize = map (\c -> if c == ' ' then '-' else c)

-- |Returns a String after default approximations for changing Unicode characters
-- to a valid ASCII range are applied. If you want to supplement the default
-- approximations with your own, you should use the transliterateCustom
-- function instead of transliterate.
transliterate :: String -> String
transliterate = transliterateCustom "?" defaultMap

-- |Returns a String after default approximations for changing Unicode characters
-- to a valid ASCII range are applied.
transliterateCustom :: String -> Transliterations -> String -> String
transliterateCustom replacement ts = concatMap lookupCharTransliteration
  where lookupCharTransliteration c =
          if isAscii c then -- Don't bother looking up Chars in ASCII range
            [c]
          else
            case Map.lookup c ts of
              Nothing  -> replacement
              Just val -> val
