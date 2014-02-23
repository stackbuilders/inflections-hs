{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections
    ( dasherize
    , parameterize
    , defaultTransliterations
    ) where

import Data.Char (toLower, isAsciiLower, isAsciiUpper, isAscii, isDigit)
import qualified Text.Parsec as P
import Control.Applicative
import qualified Text.ParserCombinators.Parsec.Char as C
import Data.List (group)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

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
-- 'pretty' URL.
parameterize :: Transliterations -> String -> String
parameterize ts s =
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


-- Private functions

-- |Matches 'acceptable' characters for parameterization purposes.
acceptableParser :: P.Stream s m Char => P.ParsecT s u m PChar
acceptableParser = do
  c <- C.satisfy isValidParamChar
  return $ Acceptable [c]

parameterizableString :: P.Stream s m Char => P.ParsecT s u m [PChar]
parameterizableString = P.many $ P.choice [
           acceptableParser
         , UCase      <$> C.satisfy isAsciiUpper
         , C.char '-' >> return Separator
         , C.char '_' >> return Underscore
         , OtherAscii <$> C.satisfy isAscii
         , NonAscii   <$> C.satisfy (not . isAscii)
         ]

-- |Look up character in transliteration list.
transliterate :: Transliterations -> Char -> Maybe PChar
transliterate ts c =
    case Map.lookup c ts of
      Just v  -> -- We may have expanded into multiple characters during
                 -- transliteration, so check validity of all characters in
                 -- result.
                 if all isValidParamChar v then
                     Just $ Acceptable v
                 else
                     Nothing

      Nothing -> Nothing

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
parameterizeChar ts (NonAscii c)   = transliterate ts c

-- |Turns PChar tokens into their String representation.
pCharToC :: PChar -> String
pCharToC (UCase c)        = [c]
pCharToC (Acceptable str) = str
pCharToC Separator        = "-"
pCharToC Underscore       = "_"
pCharToC (OtherAscii c)   = [c]
pCharToC (NonAscii c)     = [c]

-- |These default transliterations stolen from the Ruby i18n library -
-- https://github.com/svenfuchs/i18n/blob/master/lib/i18n/backend/transliterator.rb#L41:L69
defaultTransliterations :: Map.Map Char String
defaultTransliterations = Map.fromList [
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

-- |Reduce sequences of separators down to only one separator.
squeezeSeparators :: [PChar] -> [PChar]
squeezeSeparators ps = concatMap squashSeparatorGroup $ group ps
    where squashSeparatorGroup g = case head g of
                                     Separator -> [Separator] -- only take head
                                     _         -> g           -- don't change

-- |Trim non-wanted elements from the beginning and end of list.
trimUnwanted :: Eq a => [a] -> [a] -> [a]
trimUnwanted wanted xs = dropWhile notWanted $ reverse $ dropWhile notWanted $
                         reverse xs
  where notWanted elt = elt `notElem` wanted
