module Text.Inflections.Titleize (titleize) where

import Text.Inflections.Parse.Types (Word(..))

import Data.Char (toUpper)

import Prelude (String, unwords, map, ($))

-- | Capitalizes all the Words in the input 'Data.List'.
titleize
  :: [Word] -- ^ List of Words, first of which will be capitalized
  -> String -- ^ The titleized String
titleize s = unwords $ map upperCaseWord s

upperCaseWord :: Word -> String
upperCaseWord (Word (c:cs))  = toUpper c : cs
upperCaseWord (Word [])      = []
upperCaseWord (Acronym s)    = s  -- Acronyms are left intact
