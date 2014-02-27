{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections
    ( dasherize
    , parameterize
    , transliterate
    , transliterateCustom
    , Data.defaultMap
    ) where

import Data.Char (toLower, isAsciiLower, isAsciiUpper, isAscii, isDigit)
import qualified Text.Parsec as P
import Control.Applicative
import Control.Monad (guard)
import qualified Text.ParserCombinators.Parsec.Char as C
import Data.List (group)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

import qualified Text.Inflections.Data as Data

-- |A Map containing mappings from international characters to sequences
-- approximating these characters within the ASCII range.
type Transliterations = Map.Map Char String

data PChar =   UCase Char
             -- Since some of the transliterating approximations expand from
             -- one Unicode to two ASCII chars (eg., œ to oe), we represent
             -- this as a String.
             | Acceptable String
             | Separator
             | Underscore
             | OtherAscii Char
             | NonAscii Char
             deriving (Eq, Show)

-- |Replaces special characters in a string so that it may be used as part of a
-- 'pretty' URL. Uses the default transliterations in this library
parameterize :: String -> String
parameterize = parameterizeCustom Data.defaultMap

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

    where parsed = P.parse parameterizableString  "" s
          wanted :: [PChar] -- All valid URL chars - we shouldn't trim these.
          wanted = Underscore :
                   map (Acceptable . (: [])) (['a'..'z'] ++ ['0'..'9'])

-- |Replaces underscores with dashes in the string.
dasherize :: String -> String
dasherize = map (\c -> if c == ' ' then '-' else c)

-- |Returns a String after default approximations for changing Unicode characters
-- to a valid ASCII range are applied. If you want to supplement the default
-- approximations with your own, you should use the transliterateCustom
-- function instead of transliterate.
transliterate :: String -> String
transliterate = transliterateCustom "?" Data.defaultMap

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


-- Private functions


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

-- |Matches 'acceptable' characters for parameterization purposes.
acceptableParser :: P.Stream s m Char => P.ParsecT s u m PChar
acceptableParser = do
  c <- C.satisfy isValidParamChar
  return $ Acceptable [c]

parameterizableString :: P.Stream s m Char => P.ParsecT s u m [PChar]
parameterizableString = P.many $ P.choice [
           acceptableParser
         , UCase      <$> C.satisfy isAsciiUpper
         , Separator  <$  C.char '-'
         , Underscore <$  C.char '_'
         , OtherAscii <$> C.satisfy isAscii
         , NonAscii   <$> C.satisfy (not . isAscii)
         ]

isValidParamChar :: Char -> Bool
isValidParamChar c = isAsciiLower c || isDigit c

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
