-- |
-- Module      :  Text.Inflections.Camelize
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversion to CamelCased phrases.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Camelize
  ( camelize
  , camelizeCustom )
where

import Data.Text (Text)
import Text.Inflections.Types
import qualified Data.Text as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | Turn an input word list in into CamelCase.
--
-- >>> foo  <- SomeWord <$> mkWord "foo"
-- >>> bar  <- SomeWord <$> mkAcronym "bar"
-- >>> bazz <- SomeWord <$> mkAcronym "bazz"
-- >>> camelize [foo,bar,bazz]
-- "FoobarBazz"
camelize
  :: [SomeWord] -- ^ Input words
  -> Text       -- ^ The camelized 'Text'
camelize = camelizeCustom True

-- | Turn an input word list into a CamelCase String.
--
-- >>> foo  <- SomeWord <$> mkWord "foo"
-- >>> bar  <- SomeWord <$> mkAcronym "bar"
-- >>> bazz <- SomeWord <$> mkAcronym "bazz"
-- >>> camelizeCustom False [foo,bar,bazz]
-- "foobarBazz"
camelizeCustom
  :: Bool       -- ^ Whether to capitalize the first character in the output String
  -> [SomeWord] -- ^ The input Words
  -> Text       -- ^ The camelized 'Text'
camelizeCustom _ []     = ""
camelizeCustom c (x:xs) = T.concat $
  unSomeWord (if c then T.toTitle else T.toLower) x : (unSomeWord T.toTitle <$> xs)
