{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections.Parse.CamelCase ( parser )
where

import qualified Text.ParserCombinators.Parsec.Char as C
import qualified Text.Parsec as P

import Text.Inflections.Parse.Types (Word(..))
import Text.Inflections.Parse.Acronym (acronym)

-- |Recognizes an input String in CamelCase.
parser :: P.Stream s m Char => [String] -> P.ParsecT s u m [Word]
parser acronyms = do
  ws <- P.many $ P.choice [ acronym acronyms, word ]
  P.eof
  return ws

word :: P.Stream s m Char => P.ParsecT s u m Word
word = do
  firstChar <- C.upper P.<|> C.lower
  restChars <- P.many C.lower
  return $ Word $ firstChar : restChars
