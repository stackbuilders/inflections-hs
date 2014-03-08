module Text.Inflections.Titleize (titleize) where

import Text.Inflections.Parse.Types (Word(..))

import Data.List (intercalate)
import Data.Char (toUpper)

-- |Capitalizes Capitalizes all the words.
titleize
  :: [Word] -- ^ List of Words, first of which will be capitalized
  -> String -- ^ The titleized String
titleize s = intercalate " " $ map upperCaseWord s

upperCaseWord :: Word -> String
upperCaseWord (Word (c:cs))  = toUpper c : cs
upperCaseWord (Word [])      = []
upperCaseWord (Acronym s)    = s  -- Acronyms are left intact
