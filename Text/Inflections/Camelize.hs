module Text.Inflections.Camelize
  ( camelize
  , camelizeCustom )
where

import Text.Inflections.Parse.SnakeCase (Word(..), parser)
import Text.Parsec (ParseError, parse)
import Data.Char (toUpper)

camelize :: String -> Either ParseError String
camelize s = camelizeCustom [] True s

camelizeCustom :: [String] -> Bool -> String -> Either ParseError String
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
