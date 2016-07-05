-- |
-- Module      :  Text.Inflections.Parametrize
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parametrization for strings, useful for transliteration.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Parameterize
  ( parameterize
  , parameterizeCustom )
where

import Data.Char (isAscii, isAlphaNum, isPunctuation, toLower)
import Data.Text (Text)
import Text.Inflections.Data
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- |Replaces special characters in a string so that it may be used as part of a
-- 'pretty' URL. Uses the default transliterations in this library.
parameterize :: Text -> Text
parameterize = parameterizeCustom defaultMap
{-# INLINE parameterize #-}

-- |Transliterate 'Text' with a custom transliteration table.
parameterizeCustom :: Transliterations -> Text -> Text
parameterizeCustom m txt = (T.intercalate "-" . T.words) (T.unfoldr f ("", txt))
  where
    f ("", t) = uncurry g <$> T.uncons t
    f ((x:xs), t) = Just (x, (xs, t))
    g x xs
      | (isAscii x && isAlphaNum x) || x == '_' = (toLower x, ("", xs))
      | isPunctuation x = (' ', ("", xs))
      | otherwise =
          case toLower <$> M.lookupDefault " " x m of
            ""     -> (' ', ("",xs))
            (y:ys) -> (y,   (ys,xs))
