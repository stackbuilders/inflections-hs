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
import Data.Char (isAlphaNum)
import Data.Data (Data)
import Data.Void (Void)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Megaparsec
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#endif

-- | Create a word from given 'Text'. The input should consist of only
-- alpha-numeric characters (no white spaces or punctuation)
-- 'InflectionInvalidWord' will be thrown.
--
-- /since 0.3.0.0/

mkWord :: MonadThrow m => Text -> m (Word 'Normal)
mkWord txt =
  if T.all isAlphaNum txt
    then return (Word txt)
    else throwM (InflectionInvalidWord txt)

-- | Create an acronym from given 'Text'. The input should consist of only
-- alpha-numeric characters 'InflectionInvalidAcronym' will be thrown.
-- Acronym is different from normal word by that it may not be transformed
-- by inflections (also see 'unSomeWord').
--
-- /since 0.3.0.0/

mkAcronym :: MonadThrow m => Text -> m (Word 'Acronym)
mkAcronym txt =
  if T.all isAlphaNum txt
    then return (Word txt)
    else throwM (InflectionInvalidAcronym txt)

-- | A 'Text' value that should be kept whole through applied inflections.

data Word (t :: WordType) = Word Text
  deriving (Eq, Ord)

instance Show (Word 'Normal) where
  show (Word x) = "Word " ++ show x

instance Show (Word 'Acronym) where
  show (Word x) = "Acronym " ++ show x

-- | A type-level tag for words.
--
-- /since 0.3.0.0/

data WordType = Normal | Acronym

-- | Get a 'Text' value from 'Word'.
--
-- /since 0.3.0.0/

unWord :: Word t -> Text
unWord (Word s) = s

-- | An existential wrapper that allows to keep words and acronyms in single
-- list for example. The only thing that receiver of 'SomeWord' can do is to
-- apply 'unWord' on it, of course. This is faciliated by 'unSomeWord'.
--
-- /since 0.3.0.0/

data SomeWord where
  SomeWord :: (Transformable (Word t), Show (Word t)) => Word t -> SomeWord
  -- NOTE The constraint is only needed because GHC is not smart enough
  -- (yet?) to figure out that t cannot be anything other than Normal and
  -- Acronym and thus all cases are already covered by the instances
  -- provided below.

instance Show SomeWord where
  show (SomeWord w) = show w

instance Eq SomeWord where
  x == y = unSomeWord id x == unSomeWord id y

-- | Extract 'Text' from 'SomeWord' and apply given function only if the
-- word inside wasn't an acronym.
--
-- /since 0.3.0.0/

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
--
-- /since 0.3.0.0/

data InflectionException
  = InflectionParsingFailed (ParseError Char Void)
  | InflectionInvalidWord Text
  | InflectionInvalidAcronym Text
  deriving (Eq, Show, Typeable, Data, Generic)

instance Exception InflectionException
