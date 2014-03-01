module Text.Inflections.Camelize
  ( camelize
  , camelizeCustom )
where

import Text.Inflections.Parse.SnakeCase (Word(..), parser)
import Text.Parsec (ParseError, parse)
import Data.Char (toUpper)

-- |Turns a String in snake_case into CamelCase. Returns the CamelCase string,
-- or a ParseError if the input String is not in valid snake_case.
camelize
  :: String -- ^ The input string, in snake_case
  -> Either ParseError String
camelize s = camelizeCustom [] True s

-- |Turns an input String in snake_case into CamelCase. Returns the CamelCase
-- String, or a ParseError if the input String is not in valid snake_case.
-- Accepts arguments to control parsing and output format.
camelizeCustom
  :: [String] -- ^ A list of acronyms that will be kept whole and accepted as valid input
  -> Bool     -- ^ Whether to capitalize the first character in the output String
  -> String   -- ^ The input string, in snake_case
  -> Either ParseError String
camelizeCustom acronyms isFirstCap s =
  case parse (parser acronyms) "(unknown)" s of
    Left errs -> Left errs
    Right res -> Right $ concatMap (caseForWord isFirstCap) $ isFirstList res

caseForWord :: Bool -> (Word, Bool) -> String
caseForWord True (Word (c:cs), True) = toUpper c : cs
caseForWord False (Word s, True)     = s
caseForWord _ (Word (c:cs), _)       = toUpper c : cs
caseForWord _ (Word [], _)           = []
caseForWord _ (Acronym s, _)         = s

-- |Returns list with Bool indicating if an element is first.
isFirstList :: [a] -> [(a, Bool)]
isFirstList xs = zip xs $ True : repeat False
