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
-- >>> foo  <- SomeWord <$> mkWord "foo"
-- >>> bar  <- SomeWord <$> mkAcronym "bar"
-- >>> bazz <- SomeWord <$> mkWord "bazz"
-- >>> titleize [foo,bar,bazz]
-- "Foo bar Bazz"
titleize
  :: [SomeWord] -- ^ List of words, first of which will be capitalized
  -> Text       -- ^ The titleized 'Text'
titleize = T.unwords . fmap (unSomeWord T.toTitle)
