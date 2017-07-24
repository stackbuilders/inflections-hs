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
  ( humanize
  , humanizeCustom )
where

import Data.Text (Text)
import Text.Inflections.Types
import qualified Data.Text as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | Capitalize the first word and separate words with spaces. Like
-- 'Text.Inflections.Titleize.titleize', this is meant for creating pretty
-- output.
--
-- >>> foo  <- SomeWord <$> mkWord "foo"
-- >>> bar  <- SomeWord <$> mkAcronym "bar"
-- >>> bazz <- SomeWord <$> mkWord "bazz"
-- >>> humanize [foo,bar,bazz]
-- "Foo bar bazz"
humanize
  :: [SomeWord]  -- ^ List of words, first of which will be capitalized
  -> Text        -- ^ The humanized output
humanize = humanizeCustom True


-- | Separate words with spaces, optionally capitalizing the first word. Like
-- 'Text.Inflections.Titleize.titleize', this is meant for creating pretty
-- output.
--
-- >>> foo  <- SomeWord <$> mkWord "foo"
-- >>> bar  <- SomeWord <$> mkAcronym "bar"
-- >>> bazz <- SomeWord <$> mkWord "bazz"
-- >>> humanizeCustom True [foo,bar,bazz]
-- "Foo bar bazz"
-- >>> humanizeCustom False [foo,bar,bazz]
-- "foo bar bazz"
--
-- /since 0.3.0.0/
humanizeCustom
  :: Bool       -- ^ Whether to capitalize the first character in the output String
  -> [SomeWord]  -- ^ List of words, first of which will be capitalized
  -> Text        -- ^ The humanized output
humanizeCustom c xs' =
  case unSomeWord (T.replace "_" " ") <$> xs' of
    []     -> ""
    (x:xs) -> T.unwords $ (if c then T.toTitle else T.toLower) x : (T.toLower <$> xs)
