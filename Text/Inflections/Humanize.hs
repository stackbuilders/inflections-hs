-- |
-- Module      :  Text.Inflections.Humanize
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversion to “humanized” phrases.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Humanize
  ( humanize )
where

import Data.Text (Text)
import Text.Inflections.Types
import qualified Data.Text as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- |Capitalizes the first word and turns underscores into spaces. Like
-- 'Text.Inflections.Titleize.titleize', this is meant for creating pretty
-- output.
--
-- >>> humanize [Word "foo", Acronym "bar", Word "bazz"]
-- "Foo bar bazz"
--
-- Note that as of version 0.3.0.0 @Word@ and @Acronym@ constructors are
-- hidden, but you still can construct them with 'mkWord' and 'mkAcronym'.
humanize
  :: [SomeWord]  -- ^ List of Words, first of which will be capitalized
  -> Text        -- ^ The humanized output
humanize xs' =
  case unSomeWord (T.replace "_" " ") <$> xs' of
    []     -> ""
    (x:xs) -> T.unwords $ T.toTitle x : (T.toLower <$> xs)
