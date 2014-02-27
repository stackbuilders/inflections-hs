{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections.Parse.Parameterizable
  ( parser
  , isValidParamChar
  , PChar(..) )
where

import Data.Char (isAsciiLower, isAsciiUpper, isAscii, isDigit)
import Control.Applicative
import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec.Char as C

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

-- |Matches 'acceptable' characters for parameterization purposes.
acceptableParser :: P.Stream s m Char => P.ParsecT s u m PChar
acceptableParser = do
  c <- C.satisfy isValidParamChar
  return $ Acceptable [c]

parser :: P.Stream s m Char => P.ParsecT s u m [PChar]
parser = P.many $ P.choice [
           acceptableParser
         , UCase      <$> C.satisfy isAsciiUpper
         , Separator  <$  C.char '-'
         , Underscore <$  C.char '_'
         , OtherAscii <$> C.satisfy isAscii
         , NonAscii   <$> C.satisfy (not . isAscii)
         ]

isValidParamChar :: Char -> Bool
isValidParamChar c = isAsciiLower c || isDigit c
