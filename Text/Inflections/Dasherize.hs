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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Dasherize ( dasherize ) where

import Data.Text (Text)
import Text.Inflections.Parse.Types
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#endif

-- | Replaces underscores in a snake_cased string with dashes (hyphens).
--
-- >>> dasherize [ Word "foo", Acronym "bar", Word "bazz" ]
-- "foo-bar-bazz"
dasherize
  :: [Word] -- ^ Input Words to separate with dashes
  -> Text   -- ^ The dasherized String
dasherize = T.intercalate "-" . fmap (mapWord T.toLower)
{-# INLINE dasherize #-}
