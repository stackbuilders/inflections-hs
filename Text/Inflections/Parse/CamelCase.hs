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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Parse.CamelCase
  ( parseCamelCase )
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

-- | Parse a CamelCase string.
--
-- >>> parseCamelCase ["Bar"] "FooBarBazz"
-- Right [Word "Foo",Acronym "Bar",Word "Bazz"]
-- >>> parseCamelCase [] "foo_bar_bazz"
-- Left "(unknown)" (line 1, column 4):
-- unexpected '_'
parseCamelCase
  :: [Word 'Acronym]   -- ^ Collection of acronyms
  -> Text              -- ^ Input
  -> Either (ParseError Char Dec) [SomeWord] -- ^ Result of parsing
parseCamelCase acronyms = parse (parser acronyms) ""

parser
  :: [Word 'Acronym]   -- ^ Collection of acronyms
  -> Parser [SomeWord] -- ^ CamelCase parser
parser acronyms = many (a <|> n) <* eof
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
  firstChar <- upperChar <|> lowerChar
  restChars <- many $ lowerChar <|> digitChar
  case (mkWord . T.pack) (firstChar : restChars) of
    Nothing -> empty -- cannot happen if the system is sound
    Just wrd -> return wrd
{-# INLINE word #-}
