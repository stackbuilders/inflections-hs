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

# if MIN_VERSION_base(4,8,0)
import Control.Applicative (empty, some)
#else
import Control.Applicative (empty, some, (<$>), (<*))
#endif
import Data.Text (Text)
import Data.Void (Void)
import Text.Inflections.Types
import Text.Megaparsec (Parsec, ParseErrorBundle, eof, sepBy, parse)
import Text.Megaparsec.Char
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#else
import Data.Foldable
import Prelude hiding (elem)
#endif

type Parser = Parsec Void Text

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
  -> Either (ParseErrorBundle Text Void) [SomeWord] -- ^ Result of parsing
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
