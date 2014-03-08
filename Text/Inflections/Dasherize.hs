module Text.Inflections.Dasherize ( dasherize ) where

import Text.Inflections.Parse.Types (Word(..))

import Data.List (intercalate)

-- | Replaces underscores in a snake_cased string with dashes (hyphens).
dasherize
  :: [Word] -- ^ Input Words to separate with dashes
  -> String -- ^ The dasherized String
dasherize = intercalate "-" . map toString

toString :: Word -> String
toString (Acronym s) = s
toString (Word s)    = s
