-- |
-- Module      :  Text.Inflections.Parametrize
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parametrization for strings, useful for transliteration.

module Text.Inflections.Parameterize
  ( parameterize
  , parameterizeCustom
  , Transliterations )
where

import qualified Data.Map as Map
import Control.Monad (guard)
import Data.Maybe (mapMaybe)

import Data.Char (toLower)

import Data.List (group)
import qualified Text.Parsec as P

import Text.Inflections.Data (defaultMap)
import Text.Inflections.Parse.Parameterizable ( PChar(..)
                                              , parser
                                              , isValidParamChar )

-- |A 'Data.Map.Map' containing mappings from international characters to
-- sequences approximating these characters within the ASCII range.
type Transliterations = Map.Map Char String

-- |Replaces special characters in a string so that it may be used as part of a
-- 'pretty' URL. Uses the default transliterations in this library
parameterize :: String -> String
parameterize = parameterizeCustom defaultMap

-- |Transliterate a String with a custom transliteration table.
parameterizeCustom :: Transliterations -> String -> String
parameterizeCustom ts s =
    case parsed of
      Right ast -> (concatMap pCharToC . squeezeSeparators .
                    trimUnwanted wanted . mapMaybe (parameterizeChar ts))
                   ast

      -- Note that this should never fail, since we accommodate all Unicode
      -- characters as valid input.
      Left err -> fail $ "Parse failed, please report a bug! Error: " ++
                         show err

    where parsed = P.parse parser  "" s
          wanted :: [PChar] -- All valid URL chars - we shouldn't trim these.
          wanted = Underscore :
                   map (Acceptable . (: [])) (['a'..'z'] ++ ['0'..'9'])

-- |Look up character in transliteration list. Accepts a Transliteration map
-- which has Chars as keys and Strings as values for approximating common
-- international Unicode characters within the ASCII range.
transliteratePCharCustom :: Transliterations -> Char -> Maybe PChar
transliteratePCharCustom ts c = do
  -- We may have expanded into multiple characters during
  -- transliteration, so check validity of all characters in
  -- result.
  v <- Map.lookup c ts
  guard (all isValidParamChar v)
  return (Acceptable v)

-- |Given a Transliteration table and a PChar, returns Maybe PChar indicating
-- how this character should appear in a URL.
parameterizeChar :: Transliterations -> PChar -> Maybe PChar
parameterizeChar _  (UCase c)      = Just $ Acceptable [toLower c]
parameterizeChar _  (Acceptable c) = Just $ Acceptable c
parameterizeChar _  Separator      = Just Separator
parameterizeChar _  Underscore     = Just Underscore
parameterizeChar _  (OtherAscii _) = Just Separator
parameterizeChar ts (NonAscii c)   = transliteratePCharCustom ts c

-- |Turns PChar tokens into their String representation.
pCharToC :: PChar -> String
pCharToC (UCase c)        = [c]
pCharToC (Acceptable str) = str
pCharToC Separator        = "-"
pCharToC Underscore       = "_"
pCharToC (OtherAscii c)   = [c]
pCharToC (NonAscii c)     = [c]

-- |Reduce sequences of separators down to only one separator.
squeezeSeparators :: [PChar] -> [PChar]
squeezeSeparators ps = concatMap squashSeparatorGroup $ group ps
    where squashSeparatorGroup g = case head g of
                                     Separator -> [Separator] -- only take head
                                     _         -> g           -- don't change

-- |Trim non-wanted elements from the beginning and end of list.
trimUnwanted :: Eq a => [a] -> [a] -> [a]
trimUnwanted wanted = dropWhile notWanted . reverse . dropWhile notWanted
                      . reverse
  where notWanted = (`notElem` wanted)
