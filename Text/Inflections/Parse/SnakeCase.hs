{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections.Parse.SnakeCase ( Word(..), parser )
where

import qualified Text.ParserCombinators.Parsec.Char as C
import qualified Text.Parsec as P
import qualified Text.Parsec.Prim as Prim

data Word = Word String
          | Acronym String deriving (Show, Eq)

acronym :: P.Stream s m Char => [String] -> P.ParsecT s u m Word
acronym as = do
  a <- P.choice $ map (Prim.try . C.string) as
  return $ Acronym a

word :: P.Stream s m Char => P.ParsecT s u m Word
word = do
  cs <- P.many1 C.lower
  return $ Word cs

parser :: P.Stream s m Char => [String] -> P.ParsecT s u m [Word]
parser acronyms = do
  ws <- (acronym acronyms P.<|> word) `P.sepBy` (C.char '_')
  P.eof
  return ws
