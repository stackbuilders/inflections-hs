-- |
-- Module      :  Text.Inflections.Types
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used in the library. Usually you don't need to import this module
-- and "Text.Inflections" should be enough.

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}

module Text.Inflections.Types
  ( Word
  , WordType (..)
  , mkWord
  , mkAcronym
  , unWord
  , SomeWord (..)
  , unSomeWord
  , InflectionException (..) )
where

import Control.Monad.Catch
import Data.Char (isSpace)
import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Megaparsec
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#endif

-- | Create a word from given 'Text'. The input should not contain spaces or
-- 'InflectionInvalidWord' will be thrown.

mkWord :: MonadThrow m => Text -> m (Word 'Normal)
mkWord txt =
  if T.any isSpace txt
    then throwM (InflectionInvalidWord txt)
    else return (Word txt)

-- | Create an acronym from given 'Text'. The input should not contain
-- spaces or 'InflectionInvalidAcronym' will be thrown. Acronym is different
-- from normal word by that it may not be transformed by inflections.

mkAcronym :: MonadThrow m => Text -> m (Word 'Acronym)
mkAcronym txt =
  if T.any isSpace txt
    then throwM (InflectionInvalidAcronym txt)
    else return (Word txt)

-- | A 'Text' value that should be kept whole through applied inflections.
data Word (t :: WordType) = Word Text
  deriving (Eq, Ord)

instance Show (Word 'Normal) where
  show (Word x) = "Word " ++ show x

instance Show (Word 'Acronym) where
  show (Word x) = "Acronym " ++ show x

-- | A type-level tag for words.

data WordType = Normal | Acronym

-- | Get a 'Text' value from 'Word'.
unWord :: Word t -> Text
unWord (Word s) = s

-- | An existential wrapper that allows to keep words and acronyms in single
-- list for example. The only thing that receiver of 'SomeWord' can do is to
-- apply 'unWord' on it, of course. This is faciliated by 'unSomeWord'.

data SomeWord where
  SomeWord :: (Transformable (Word t), Show (Word t)) => Word t -> SomeWord
  -- NOTE The constraint is only needed because GHC is not smart enough
  -- (yet?) to figure out that t cannot be anything other than Normal and
  -- Acronym and thus all cases are already covered by the instances
  -- provided below.

instance Show SomeWord where
  show (SomeWord w) = show w

-- | Extract 'Text' from 'SomeWord' and apply given function only if the
-- word inside wasn't an acronym.

unSomeWord :: (Text -> Text) -> SomeWord -> Text
unSomeWord f (SomeWord w) = transform f w

-- | Non public stuff.

class Transformable a where
  transform :: (Text -> Text) -> a -> Text

instance Transformable (Word 'Normal) where
  transform f = f . unWord

instance Transformable (Word 'Acronym) where
  transform _ = unWord

-- | The exceptions that is thrown when parsing of input fails.

data InflectionException
  = InflectionParsingFailed (ParseError Char Dec)
  | InflectionInvalidWord Text
  | InflectionInvalidAcronym Text
  deriving (Eq, Show, Typeable, Data, Generic)

instance Exception InflectionException
