-- |
-- Module      :  Text.Inflections.Underscore
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversion to phrases separated by underscores.

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

-- | Transform 'Word' into a down-cased 'String'.
toDowncasedString :: Word -> String
toDowncasedString (Acronym s) = map toLower s
toDowncasedString (Word s)    = map toLower s
