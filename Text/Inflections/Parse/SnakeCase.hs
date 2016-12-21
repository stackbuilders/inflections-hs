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
#else
import Data.Foldable
import Prelude hiding (elem)
#endif

-- | Parse a snake_case string.
--
-- >>> bar <- mkAcronym "bar"
-- >>> parseSnakeCase [bar] "foo_bar_bazz"
-- Right [Word "foo",Acronym "bar",Word "bazz"]
--
-- >>> parseSnakeCase [] "fooBarBazz"
-- 1:4:
-- unexpected 'B'
-- expecting '_', end of input, or lowercase letter
parseSnakeCase :: (Foldable f, Functor f)
  => f (Word 'Acronym) -- ^ Collection of acronyms
  -> Text              -- ^ Input
  -> Either (ParseError Char Dec) [SomeWord] -- ^ Result of parsing
parseSnakeCase acronyms = parse (parser acronyms) ""

parser :: (Foldable f, Functor f)
  => f (Word 'Acronym)
  -> Parser [SomeWord]
parser acronyms = (pWord acronyms `sepBy` char '_') <* eof

pWord :: (Foldable f, Functor f)
  => f (Word 'Acronym)
  -> Parser SomeWord
pWord acronyms = do
  let acs = unWord <$> acronyms
  r <- T.pack <$> some alphaNumChar
  if r `elem` acs
    then maybe empty (return . SomeWord) (mkAcronym r)
    else maybe empty (return . SomeWord) (mkWord    r)
