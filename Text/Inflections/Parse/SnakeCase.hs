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

{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}

module Text.Inflections.Parse.SnakeCase
  ( parseSnakeCase )
where

import Control.Applicative
import Data.Text (Text)
import Text.Inflections.Types
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#endif

-- | Parse a snake_case string.
--
-- >>> bar <- mkAcronym "bar"
-- >>> parseSnakeCase [bar] "foo_bar_bazz"
-- Right [Word "foo",Acronym "bar",Word "bazz"]
--
-- >>> parseSnakeCase [] "fooBarBazz"
-- Left "(unknown)" (line 1, column 4):
-- unexpected 'B'
parseSnakeCase
  :: [Word 'Acronym]   -- ^ Collection of acronyms
  -> Text              -- ^ Input
  -> Either (ParseError Char Dec) [SomeWord] -- ^ Result of parsing
parseSnakeCase acronyms = parse (parser acronyms) ""

parser
  :: [Word 'Acronym]
  -> Parser [SomeWord]
parser acronyms = ((a <|> n) `sepBy` char '_') <* eof
  where
    n = SomeWord <$> word
    a = SomeWord <$> acronym acronyms

acronym :: [Word 'Acronym] -> Parser (Word 'Acronym)
acronym acronyms = do
  x <- T.pack <$> choice (string . T.unpack . unWord <$> acronyms)
  case mkAcronym x of
    Nothing -> empty -- cannot happen if the system is sound
    Just acr -> return acr
{-# INLINE acronym #-}

word :: Parser (Word 'Normal)
word = do
  x <- T.pack <$> (some lowerChar <|> some digitChar)
  case mkWord x of
    Nothing -> empty -- cannot happen if the system is sound
    Just wrd -> return wrd
{-# INLINE word #-}
