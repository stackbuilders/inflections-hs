-- |
-- Module      :  Text.Inflections.Parse.Types
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used in the library.

module Text.Inflections.Parse.Types ( Word(..), mapWord ) where

import Prelude (String, Show, Eq, ($))

-- | A 'String' that should be kept whole through applied inflections
data Word

    -- | A word that may be transformed by inflection
    = Word String

    -- | A word that may not be transformed by inflections
    | Acronym String

    deriving (Show, Eq)

-- | Apply 'String' transforming function to a 'Word'.
mapWord :: (String -> String) -> Word -> Word
mapWord f (Word s) = Word $ f s
mapWord f (Acronym s) = Acronym $ f s
