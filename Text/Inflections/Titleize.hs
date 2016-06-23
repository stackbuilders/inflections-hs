-- |
-- Module      :  Text.Inflections.Titleize
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversion to titleized phrases.

module Text.Inflections.Titleize (titleize) where

import Text.Inflections.Parse.Types (Word(..))

import Data.Char (toUpper)

import Prelude (String, unwords, map, ($))

-- | Capitalizes all the Words in the input list.
--
-- >>> titleize [ Word "foo", Acronym "bar", Word "bazz" ]
-- "Foo bar Bazz"
titleize
  :: [Word] -- ^ List of Words, first of which will be capitalized
  -> String -- ^ The titleized String
titleize s = unwords $ map upperCaseWord s

-- | Transform 'Word' into an upper-cased 'String'.
upperCaseWord :: Word -> String
upperCaseWord (Word (c:cs))  = toUpper c : cs
upperCaseWord (Word [])      = []
upperCaseWord (Acronym s)    = s  -- Acronyms are left intact
