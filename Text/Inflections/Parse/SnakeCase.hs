-- |
-- Module      :  Text.Inflections.Parse.SnakeCase
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for snake case “symbols”.

{-# LANGUAGE CPP #-}

module Text.Inflections.Parse.SnakeCase ( parseSnakeCase )
where

import Text.Inflections.Parse.Acronym (acronym)
import Text.Inflections.Parse.Types (Word(..))
import Text.Megaparsec
import Text.Megaparsec.String

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#else
import Control.Applicative
#endif

-- |Parses a snake_case string.
--
-- >>> parseSnakeCase ["bar"] "foo_bar_bazz"
-- Right [Word "foo",Acronym "bar",Word "bazz"]
-- >>> parseSnakeCase [] "fooBarBazz"
-- Left "(unknown)" (line 1, column 4):
-- unexpected 'B'
parseSnakeCase
  :: [String]          -- ^ Collection of acronyms
  -> String            -- ^ Input
  -> Either (ParseError Char Dec) [Word] -- ^ Result of parsing
parseSnakeCase acronyms = parse (parser acronyms) ""

parser
  :: [String] -> Parser [Word]
parser acronyms = do
  ws <- (acronym acronyms <|> word) `sepBy` char '_'
  eof
  return ws

word :: Parser Word
word = Word <$> (some lowerChar <|> some digitChar)
