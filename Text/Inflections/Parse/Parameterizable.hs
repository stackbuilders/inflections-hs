-- |
-- Module      :  Text.Inflections.Parse.Parameterizable
-- Copyright   :  © 2014–2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parametrizable characters and parser for them.

{-# LANGUAGE CPP, FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections.Parse.Parameterizable
  ( parser
  , isValidParamChar
  , PChar(..) )
where

import Data.Char (isAsciiLower, isAsciiUpper, isAscii, isDigit)
import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec.Char as C

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | Parametrizable character.

data PChar
  = UCase Char -- ^ Uppercase charater
  | Acceptable String
  -- ^ Since some of the transliterating approximations expand from one
  -- Unicode to two ASCII chars (eg., œ to oe), we represent this as a
  -- String.
  | Separator
  | Underscore
  | OtherAscii Char
  | NonAscii Char
  deriving (Eq, Show)

-- |Matches 'acceptable' characters for parameterization purposes.
acceptableParser :: P.Stream s m Char => P.ParsecT s u m PChar
acceptableParser = do
  c <- C.satisfy isValidParamChar
  return $ Acceptable [c]

-- | Parser that accepts rows of parametrizable characters.
parser :: P.Stream s m Char => P.ParsecT s u m [PChar]
parser = P.many $ P.choice [
           acceptableParser
         , UCase      <$> C.satisfy isAsciiUpper
         , Separator  <$  C.char '-'
         , Underscore <$  C.char '_'
         , OtherAscii <$> C.satisfy isAscii
         , NonAscii   <$> C.satisfy (not . isAscii)
         ]

-- | Check if given char is “acceptable”, that is, it's lowercase ASCII
-- letter or digit.
isValidParamChar :: Char -> Bool
isValidParamChar c = isAsciiLower c || isDigit c
