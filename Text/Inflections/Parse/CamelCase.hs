{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections.Parse.CamelCase ( parser )
where

import qualified Text.ParserCombinators.Parsec.Char as C
import qualified Text.Parsec as P

import Text.Inflections.Parse.Types (Word(..))
import Text.Inflections.Parse.Acronym (acronym)

word :: P.Stream s m Char => P.ParsecT s u m Word
word = do
  firstChar <- C.upper P.<|> C.lower
  restChars <- P.many C.lower
  return $ Word $ firstChar : restChars

parser :: P.Stream s m Char => [String] -> P.ParsecT s u m [Word]
parser acronyms = do
  ws <- P.many1 $ P.choice [ acronym acronyms, word ]
  P.eof
  return ws
