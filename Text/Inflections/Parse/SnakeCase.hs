{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections.Parse.SnakeCase ( Word(..), parser )
where

import Control.Applicative ((<$>))
import Text.Parsec

import Text.Inflections.Parse.Types (Word(..))
import Text.Inflections.Parse.Acronym (acronym)

word :: Stream s m Char => ParsecT s u m Word
word = Word <$> ((many1 lower) <|> (many1 digit))

parser :: Stream s m Char => [String] -> ParsecT s u m [Word]
parser acronyms = do
  ws <- (acronym acronyms <|> word) `sepBy` (char '_')
  eof
  return ws
