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

-- |Turns a CamelCase string into an underscore_separated 'Text'.
--
-- >>> underscore [ Word "foo", Acronym "bar", Word "bazz" ]
-- "foo_bar_bazz"
underscore
  :: [SomeWord] -- ^ Input Words to separate with underscores
  -> Text       -- ^ The underscored String
underscore = T.intercalate "_" . fmap (unSomeWord T.toLower)
