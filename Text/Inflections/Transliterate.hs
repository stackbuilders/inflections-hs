-- |
-- Module      :  Text.Inflections.Transliterate
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Support for transliteration.

module Text.Inflections.Transliterate
    ( transliterate
    , transliterateCustom
    )
where

import Data.Char (isAscii)
import Data.Maybe(fromMaybe)
import Text.Inflections.Data
import qualified Data.Map as Map

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
            fromMaybe replacement (Map.lookup c ts)
