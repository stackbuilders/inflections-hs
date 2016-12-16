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

{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Dasherize
  ( dasherize )
where

import Data.Text (Text)
import Text.Inflections.Types
import qualified Data.Text as T

-- | Replaces underscores in a snake_cased string with dashes (hyphens).
--
-- >>> foo  <- SomeWord <$> mkWord "foo"
-- >>> bar  <- SomeWord <$> mkAcronym "bar"
-- >>> bazz <- SomeWord <$> mkWord "bazz"
-- >>> dasherize [foo,bar,bazz]
-- "foo-bar-bazz"
dasherize
  :: [SomeWord] -- ^ Input Words to separate with dashes
  -> Text       -- ^ The dasherized String
dasherize = T.intercalate "-" . fmap (unSomeWord T.toLower)
