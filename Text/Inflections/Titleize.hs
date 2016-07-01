-- |
-- Module      :  Text.Inflections.Titleize
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversion to titleized phrases.

{-# LANGUAGE CPP #-}

module Text.Inflections.Titleize
  ( titleize )
where

import Data.Text (Text)
import Text.Inflections.Parse.Types
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#endif

-- | Capitalize all the Words in the input list.
--
-- >>> titleize [ Word "foo", Acronym "bar", Word "bazz" ]
-- "Foo bar Bazz"
titleize
  :: [Word] -- ^ List of Words, first of which will be capitalized
  -> Text   -- ^ The titleized String
titleize = T.unwords . fmap (mapWord T.toTitle)
{-# INLINE titleize #-}
