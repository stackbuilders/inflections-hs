-- |
-- Module      :  Text.Inflections.Parse.Acronym
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for acronyms.

{-# LANGUAGE CPP #-}

module Text.Inflections.Parse.Acronym
  ( acronym )
where

import Data.Text (Text)
import Text.Inflections.Parse.Types
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (Word)
#endif

-- | Parser that accepts a 'Text' from given collection and turns it into
-- an 'Acronym'.
acronym :: [Text] -> Parser Word
acronym = fmap (Acronym . T.pack) . choice . fmap (string . T.unpack)
{-# INLINE acronym #-}
