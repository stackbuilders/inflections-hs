module Text.Inflections.Dasherize
  ( dasherize
  , dasherizeCustom )
where

import Text.Inflections.Parse.SnakeCase (Word(..), parser)
import Text.Parsec (ParseError, parse)

import Data.List (intercalate)

-- |Replaces underscores in a snake_cased string with dashes (hyphens).
dasherize
  :: String -- ^ The input string, in snake_case
  -> Either ParseError String
dasherize s = dasherizeCustom [] s

-- |Turns a snake_case string into dasherized form. Returns a ParseError if
-- the input string is not in proper snake_case.
dasherizeCustom
  :: [String] -- ^ A list of acronyms that will be kept whole and accepted as valid input
  -> String   -- ^ The input string, in snake_case
  -> Either ParseError String
dasherizeCustom acronyms s =
  case parse (parser acronyms) "(unknown)" s of
    Left errs -> Left errs
    Right res -> Right $ intercalate "-" $ map toString res


toString :: Word -> String
toString (Acronym s) = s
toString (Word s)    = s
