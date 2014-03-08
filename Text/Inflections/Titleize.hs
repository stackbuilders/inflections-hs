module Text.Inflections.Titleize (titleize, titleizeCustom) where

import Text.Inflections.Parse.SnakeCase (Word(..), parser)
import Text.Parsec (ParseError, parse)

import Data.List (intercalate)
import Data.Char (toUpper)

-- |Capitalizes Capitalizes all the words.
titleize
  :: String -- ^ The input string, in snake_case
  -> Either ParseError String
titleize s = titleizeCustom [] s

-- |Turns a snake_case string into titleize form. Returns a ParseError if
-- the input string is not in proper snake_case.
titleizeCustom
  :: [String] -- ^ A list of acronyms that will be kept whole and accepted as valid input
  -> String   -- ^ The input string, in snake_case
  -> Either ParseError String
titleizeCustom acronyms s =
  case parse (parser acronyms) "(unknown)" s of
    Left errs -> Left errs
    Right res -> Right $ intercalate " " $ map upperCaseWord res

upperCaseWord :: Word -> String
upperCaseWord (Word (c:cs))  = toUpper c : cs
upperCaseWord (Word [])      = []
upperCaseWord (Acronym s)    = s  -- Acronyms are left intact
