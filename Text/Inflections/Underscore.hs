module Text.Inflections.Underscore ( underscore ) where

import Text.Inflections.Parse.Types (Word(..))

import Data.Char (toLower)
import Data.List (intercalate)

-- |Turns a CamelCase string into an underscore_separated String.
underscore
  :: [Word] -- ^ Input Words to separate with underscores
  -> String -- ^ The underscored String
underscore ws = intercalate "_" $ map toDowncasedString ws


toDowncasedString :: Word -> String
toDowncasedString (Acronym s) = map toLower s
toDowncasedString (Word s)    = map toLower s
