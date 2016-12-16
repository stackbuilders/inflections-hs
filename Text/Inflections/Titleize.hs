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

module Text.Inflections.Titleize
  ( titleize )
where

import Data.Text (Text)
import Text.Inflections.Types
import qualified Data.Text as T

-- | Capitalize all the Words in the input list.
--
-- >>> titleize [ Word "foo", Acronym "bar", Word "bazz" ]
-- "Foo bar Bazz"
titleize
  :: [SomeWord] -- ^ List of Words, first of which will be capitalized
  -> Text       -- ^ The titleized String
titleize = T.unwords . fmap (unSomeWord T.toTitle)
