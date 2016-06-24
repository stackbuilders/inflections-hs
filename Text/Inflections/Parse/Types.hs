-- |
-- Module      :  Text.Inflections.Parse.Types
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used in the library.

{-# LANGUAGE CPP #-}

module Text.Inflections.Parse.Types
  ( Word (..) -- FIXME we should not export the constructor
  , unWord
  , mapWord )
where

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#endif

-- | A 'String' that should be kept whole through applied inflections
data Word
  = Word String -- ^ A word that may be transformed by inflection
  | Acronym String -- ^ A word that may not be transformed by inflections
  deriving (Show, Eq)

-- | Get a 'String' from 'Word'.
unWord :: Word -> String
unWord (Word    s) = s
unWord (Acronym s) = s
{-# INLINE unWord #-}

-- | Apply 'String' transforming function to a 'Word'.
mapWord :: (String -> String) -> Word -> Word
mapWord f (Word    s) = Word (f s)
mapWord f (Acronym s) = Acronym (f s)
{-# INLINE mapWord #-}
