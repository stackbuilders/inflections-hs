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
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Parse.CamelCase
  ( parseCamelCase )
where

import Data.Text (Text)
import Text.Inflections.Parse.Acronym (acronym)
import Text.Inflections.Parse.Types (Word(..))
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#else
import Control.Applicative
#endif

-- |Parse a CamelCase string.
--
-- >>> parseCamelCase ["Bar"] "FooBarBazz"
-- Right [Word "Foo",Acronym "Bar",Word "Bazz"]
-- >>> parseCamelCase [] "foo_bar_bazz"
-- Left "(unknown)" (line 1, column 4):
-- unexpected '_'
parseCamelCase
  :: [Text]          -- ^ Collection of acronyms
  -> Text            -- ^ Input
  -> Either (ParseError Char Dec) [Word] -- ^ Result of parsing
parseCamelCase acronyms = parse (parser acronyms) ""

parser
  :: [Text]            -- ^ Collection of acronyms
  -> Parser [Word]     -- ^ CamelCase parser
parser acronyms = many (acronym acronyms <|> word) <* eof
{-# INLINE parser #-}

word :: Parser Word
word = do
  firstChar <- upperChar <|> lowerChar
  restChars <- many $ lowerChar <|> digitChar
  return . Word . T.pack $ firstChar : restChars
{-# INLINE word #-}
