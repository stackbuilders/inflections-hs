-- |
-- Module      :  Text.Inflections.Dasherize
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversion to dasherized phrases.

module Text.Inflections.Dasherize ( dasherize ) where

import Text.Inflections.Parse.Types (Word(..))

import Data.List (intercalate)

import Prelude (String, (.), map)

-- | Replaces underscores in a snake_cased string with dashes (hyphens).
--
-- >>> dasherize [ Word "foo", Acronym "bar", Word "bazz" ]
-- "foo-bar-bazz"

dasherize
  :: [Word] -- ^ Input Words to separate with dashes
  -> String -- ^ The dasherized String
dasherize = intercalate "-" . map toString

-- | Covert a 'Word' into its 'String' representation.
toString :: Word -> String
toString (Acronym s) = s
toString (Word s)    = s
