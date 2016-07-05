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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Underscore
  ( underscore )
where

import Data.Text (Text)
import Text.Inflections.Parse.Types
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#endif

-- |Turns a CamelCase string into an underscore_separated 'Text'.
--
-- >>> underscore [ Word "foo", Acronym "bar", Word "bazz" ]
-- "foo_bar_bazz"
underscore
  :: [Word] -- ^ Input Words to separate with underscores
  -> Text   -- ^ The underscored String
underscore = T.intercalate "_" . fmap (mapWord T.toLower)
{-# INLINE underscore #-}
