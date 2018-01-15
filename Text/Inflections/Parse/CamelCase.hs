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

#if !MIN_VERSION_megaparsec(6,4,0)
import Control.Applicative
#endif
import Data.Text (Text)
import Data.Void (Void)
import Text.Inflections.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#else
import Data.Foldable
import Prelude hiding (elem)
#endif

type Parser = Parsec Void Text

-- | Parse a CamelCase string.
--
-- >>> bar <- mkAcronym "bar"
-- >>> parseCamelCase [bar] "FooBarBazz"
-- Right [Word "Foo",Acronym "Bar",Word "Bazz"]
--
-- >>> parseCamelCase [] "foo_bar_bazz"
-- 1:4:
-- unexpected '_'
-- expecting end of input, lowercase letter, or uppercase letter
parseCamelCase :: (Foldable f, Functor f)
  => f (Word 'Acronym) -- ^ Collection of acronyms
  -> Text              -- ^ Input
  -> Either (ParseError Char Void) [SomeWord] -- ^ Result of parsing
parseCamelCase acronyms = parse (parser acronyms) ""

parser :: (Foldable f, Functor f)
  => f (Word 'Acronym) -- ^ Collection of acronyms
  -> Parser [SomeWord] -- ^ CamelCase parser
parser acronyms = many (a <|> n) <* eof
  where
    n = SomeWord <$> word
    a = SomeWord <$> acronym acronyms

acronym :: (Foldable f, Functor f)
  => f (Word 'Acronym)
  -> Parser (Word 'Acronym)
acronym acronyms = do
  x <- choice (string . unWord <$> acronyms)
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
