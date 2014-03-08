{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections.Parse.SnakeCase ( Word(..), parser )
where

import qualified Text.ParserCombinators.Parsec.Char as C
import qualified Text.Parsec as P
import Control.Applicative ((<$>))

import Text.Inflections.Parse.Types (Word(..))
import Text.Inflections.Parse.Acronym (acronym)

word :: P.Stream s m Char => P.ParsecT s u m Word
word = Word <$> ((P.many1 C.lower) P.<|> (P.many1 C.digit))

parser :: P.Stream s m Char => [String] -> P.ParsecT s u m [Word]
parser acronyms = do
  ws <- (acronym acronyms P.<|> word) `P.sepBy` (C.char '_')
  P.eof
  return ws
