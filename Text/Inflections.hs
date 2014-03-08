{- |
Module      :  Text.Inflections
Description :  Rails-like inflections library for common String transformations.
Copyright   :  (c) Justin Leitgeb
License     :  MIT

Maintainer  :  justin@stackbuilders.com
Stability   :  unstable
Portability :  portable

This module provides methods for common String transformations, similar to the
"Inflections" library found in Rails:

<http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html>
-}

module Text.Inflections
    ( camelize
    , camelizeCustom

    , dasherize
    , dasherizeCustom

    , humanize
    , humanizeCustom

    , underscore
    , underscoreCustom

    , titleize
    , titleizeCustom

    , defaultMap

    , parameterize
    , parameterizeCustom

    , transliterate
    , transliterateCustom

    , ordinal
    , ordinalize
    )
where

import Data.Char (isAscii)
import qualified Data.Map as Map

import Text.Inflections.Data (defaultMap)

import Text.Inflections.Parameterize ( Transliterations
                                     , parameterize
                                     , parameterizeCustom )

import Text.Inflections.Underscore ( underscore, underscoreCustom )

import Text.Inflections.Camelize ( camelize, camelizeCustom )

import Text.Inflections.Humanize ( humanize, humanizeCustom )

import Text.Inflections.Titleize ( titleize, titleizeCustom )

import Text.Inflections.Dasherize ( dasherize, dasherizeCustom )

import Text.Inflections.Ordinal ( ordinal, ordinalize )

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
