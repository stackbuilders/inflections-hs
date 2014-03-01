module Text.Inflections.Decamelize (decamelize, decamelizeCustom) where

import Text.Inflections.Parse.CamelCase (Word(..), parser)
import Text.Parsec (ParseError, parse)
import Data.Char (toLower)
import Data.List (intercalate)

decamelize :: String -> Either ParseError String
decamelize s = decamelizeCustom [] s

decamelizeCustom :: [String] -> String -> Either ParseError String
decamelizeCustom acronyms s =
  case parse (parser acronyms) "(unknown)" s of
    Left errs -> Left errs
    Right res -> Right $ intercalate "_" $ map toDowncasedString res

toDowncasedString :: Word -> String
toDowncasedString w = map toLower $ wordToS w
  where wordToS (Word s)    = s
        wordToS (Acronym s) = s
