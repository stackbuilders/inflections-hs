module Text.Inflections.Underscore ( underscore ) where

import Text.Inflections.Parse.Types (Word(..))

import Data.Char (toLower)
import Data.List (intercalate)

import Prelude (String, (.), map)

-- |Turns a CamelCase string into an underscore_separated String.
--
-- >>> underscore [ Word "foo", Acronym "bar", Word "bazz" ]
-- "foo_bar_bazz"

underscore
  :: [Word] -- ^ Input Words to separate with underscores
  -> String -- ^ The underscored String
underscore = intercalate "_" . map toDowncasedString


toDowncasedString :: Word -> String
toDowncasedString (Acronym s) = map toLower s
toDowncasedString (Word s)    = map toLower s
