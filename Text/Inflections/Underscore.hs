module Text.Inflections.Underscore (underscore, underscoreCustom) where

import Text.Inflections.Parse.CamelCase (Word(..), parser)
import Text.Parsec (ParseError, parse)
import Data.Char (toLower)
import Data.List (intercalate)

underscore :: String -> Either ParseError String
underscore s = underscoreCustom [] s

underscoreCustom :: [String] -> String -> Either ParseError String
underscoreCustom acronyms s =
  case parse (parser acronyms) "(unknown)" s of
    Left errs -> Left errs
    Right res -> Right $ intercalate "_" $ map toDowncasedString res

toDowncasedString :: Word -> String
toDowncasedString (Acronym s) = map toLower s
toDowncasedString (Word s) = map toLower s

