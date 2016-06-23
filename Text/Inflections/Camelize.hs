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

module Text.Inflections.Camelize ( camelize, camelizeCustom ) where

import Text.Inflections.Parse.Types (Word(..))

import Data.Char (toUpper, toLower)

import Prelude (String, Bool(..), concatMap, (.), zip, ($), repeat)

-- |Turns a an input Word List in into CamelCase. Returns the CamelCase String.
--
-- >>> camelize [ Word "foo", Acronym "bar", Word "bazz" ]
-- "FoobarBazz"
camelize
  :: [Word] -- ^ Input Words to separate with underscores
  -> String -- ^ The camelized String
camelize = camelizeCustom True

-- |Turns an input Word List into a CamelCase String.
--
-- >>> camelizeCustom False [ Word "foo", Acronym "bar", Word "bazz" ]
-- "foobarBazz"
camelizeCustom
  :: Bool   -- ^ Whether to capitalize the first character in the output String
  -> [Word] -- ^ The input Words
  -> String -- ^ The camelized String
camelizeCustom isFirstCap = concatMap (caseForWord isFirstCap) . isFirstList

caseForWord :: Bool -> (Word, Bool) -> String
caseForWord True (Word (c:cs), True)  = toUpper c : cs
caseForWord False (Word (c:cs), True) = toLower c : cs
caseForWord _ (Word (c:cs), _)        = toUpper c : cs
caseForWord _ (Word [], _)            = []
caseForWord _ (Acronym s, _)          = s

-- |Returns list with Bool indicating if an element is first.
isFirstList :: [a] -> [(a, Bool)]
isFirstList xs = zip xs $ True : repeat False
