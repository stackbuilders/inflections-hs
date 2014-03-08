module Text.Inflections.Humanize (humanize, humanizeCustom) where

import Text.Inflections.Parse.SnakeCase (Word(..), parser)
import Text.Parsec (ParseError, parse)

import Data.List (intercalate)
import Data.Char (toUpper)

-- |Capitalizes the first word and turns underscores into spaces Like titleize,
-- this is meant for creating pretty output.
humanize
  :: String -- ^ The input string, in snake_case
  -> Either ParseError String
humanize s = humanizeCustom [] s

-- |Turns a snake_case string into humanize form. Returns a ParseError if
-- the input string is not in proper snake_case.
humanizeCustom
  :: [String] -- ^ A list of acronyms that will be kept whole and accepted as valid input
  -> String   -- ^ The input string, in snake_case
  -> Either ParseError String
humanizeCustom acronyms s =
  case parse (parser acronyms) "(unknown)" s of
    Left errs -> Left errs
    Right res -> Right $ intercalate " " $ map caseForWord $ isFirstList res

-- |Returns list with Bool indicating if an element is first.
isFirstList :: [a] -> [(a, Bool)]
isFirstList xs = zip xs $ True : repeat False

caseForWord :: (Word, Bool) -> String
caseForWord (Word (c:cs), True)  = toUpper c : cs
caseForWord (Word s, False)      = s
caseForWord (Word [], _)         = []
caseForWord (Acronym s, _)       = s  -- Acronyms are left intact
