module Text.Inflections.Underscore (underscore, underscoreCustom) where

import Text.Inflections.Parse.CamelCase (parser)
import Text.Inflections.Parse.Types (Word(..))

import Text.Parsec (ParseError, parse)
import Data.Char (toLower)
import Data.List (intercalate)

-- |Turns a CamelCase string into an underscore_separated String.
underscore
  :: String -- ^ A String in CamelCase
  -> Either ParseError String
underscore s = underscoreCustom [] s

-- |Changes an input String in CamelCase into a String of Words separated by
-- underscores. Accepts options for customization.
underscoreCustom
  :: [String] -- ^ A list of acronyms that will be kept whole and accepted as valid input
  -> String -- ^ A String in CamelCase
  -> Either ParseError String
underscoreCustom acronyms s =
  case parse (parser acronyms) "(unknown)" s of
    Left errs -> Left errs
    Right res -> Right $ intercalate "_" $ map toDowncasedString res


toDowncasedString :: Word -> String
toDowncasedString (Acronym s) = map toLower s
toDowncasedString (Word s) = map toLower s

