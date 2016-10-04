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
import Text.Inflections.Parse.Types
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#else
import Control.Applicative
#endif

-- |Turns a an input Word List in into CamelCase. Returns the CamelCase String.
--
-- >>> camelize [ Word "foo", Acronym "bar", Word "bazz" ]
-- "FoobarBazz"
camelize
  :: [Word] -- ^ Input Words to separate with underscores
  -> Text   -- ^ The camelized 'Text'
camelize = camelizeCustom True
{-# INLINE camelize #-}

-- |Turns an input Word List into a CamelCase String.
--
-- >>> camelizeCustom False [ Word "foo", Acronym "bar", Word "bazz" ]
-- "foobarBazz"
camelizeCustom
  :: Bool   -- ^ Whether to capitalize the first character in the output String
  -> [Word] -- ^ The input Words
  -> Text   -- ^ The camelized 'Text'
camelizeCustom _ []     = ""
camelizeCustom c (x:xs) = T.concat $
  mapWord (if c then T.toTitle else T.toLower) x : (mapWord T.toTitle <$> xs)
{-# INLINE camelizeCustom #-}
