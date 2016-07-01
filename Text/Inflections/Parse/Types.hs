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

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Text.Inflections.Parse.Types
  ( Word (..) -- FIXME we should not export the constructor
  , unWord
  , mapWord
  , InflectionException (..) )
where

import Control.Monad.Catch (Exception)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Text.Megaparsec

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#endif

-- | A 'String' that should be kept whole through applied inflections
data Word
  = Word    Text -- ^ A word that may be transformed by inflection
  | Acronym Text -- ^ A word that may not be transformed by inflections
  deriving (Show, Eq)

-- | Get a 'Text' value from 'Word'.
unWord :: Word -> Text
unWord (Word    s) = s
unWord (Acronym s) = s
{-# INLINE unWord #-}

-- | Apply 'Text' transforming function to a 'Word' unless it's a 'Acronym'.
mapWord :: (Text -> Text) -> Word -> Text
mapWord f (Word    s) = f s
mapWord _ (Acronym s) = s
{-# INLINE mapWord #-}

-- | The exceptions that is thrown when parsing of input fails.

data InflectionException = InflectionParsingFailed (ParseError Char Dec)
  deriving (Eq, Show, Typeable)

instance Exception InflectionException
