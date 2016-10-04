-- |
-- Module      :  Text.Inflections.Transliterate
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Support for transliteration.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Transliterate
    ( transliterate
    , transliterateCustom )
where

import Data.Char (isAscii)
import Data.Text (Text)
import Text.Inflections.Data
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- |Returns a String after default approximations for changing Unicode
-- characters to a valid ASCII range are applied. If you want to supplement
-- the default approximations with your own, you should use the
-- 'transliterateCustom' function instead of 'transliterate'.
transliterate :: Text -> Text
transliterate = transliterateCustom "?" defaultMap
{-# INLINE transliterate #-}

-- |Returns a String after default approximations for changing Unicode
-- characters to a valid ASCII range are applied.
transliterateCustom
  :: String            -- ^ The default replacement
  -> Transliterations  -- ^ The table of transliterations
  -> Text              -- ^ The input
  -> Text              -- ^ The output
transliterateCustom replacement m txt = T.unfoldr f ("", txt)
  where
    f ("", t) = uncurry g <$> T.uncons t
    f (x:xs, t) = Just (x, (xs, t))
    g x xs =
      if isAscii x
        then (x, ("", xs))
        else
          case M.lookupDefault replacement x m of
            ""     -> ('?', ("",xs))
            (y:ys) -> (y,   (ys,xs))
