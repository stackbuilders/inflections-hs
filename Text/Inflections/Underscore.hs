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

{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Underscore
  ( underscore )
where

import Data.Text (Text)
import Text.Inflections.Types
import qualified Data.Text as T

-- | Separate given words by underscores.
--
-- >>> foo  <- SomeWord <$> mkWord "foo"
-- >>> bar  <- SomeWord <$> mkAcronym "bar"
-- >>> bazz <- SomeWord <$> mkWord "bazz"
-- >>> underscore [foo,bar,bazz]
-- "foo_bar_bazz"
underscore
  :: [SomeWord] -- ^ Input words to separate with underscores
  -> Text       -- ^ The underscored String
underscore = T.intercalate "_" . fmap (unSomeWord T.toLower)
