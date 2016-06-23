-- |
-- Module      :  Text.Inflections.Parse.CamelCase
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for camel case “symbols”.

{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections.Parse.CamelCase ( parseCamelCase )
where

import Text.Parsec

import Text.Inflections.Parse.Types (Word(..))
import Text.Inflections.Parse.Acronym (acronym)

import Prelude (Char, String, Either, return, ($))

-- |Parses a CamelCase string.
--
-- >>> parseCamelCase ["Bar"] "FooBarBazz"
-- Right [Word "Foo",Acronym "Bar",Word "Bazz"]
-- >>> parseCamelCase [] "foo_bar_bazz"
-- Left "(unknown)" (line 1, column 4):
-- unexpected '_'
parseCamelCase :: [String] -> String -> Either ParseError [Word]
parseCamelCase acronyms = parse (parser acronyms) "(unknown)"

-- |Recognizes an input String in CamelCase.
parser :: Stream s m Char => [String] -> ParsecT s u m [Word]
parser acronyms = do
  ws <- many $ choice [ acronym acronyms, word ]
  eof
  return ws

-- | Parser that accepts lower-cased and capitalized words.
word :: Stream s m Char => ParsecT s u m Word
word = do
  firstChar <- upper <|> lower
  restChars <- many lower
  return $ Word $ firstChar : restChars
