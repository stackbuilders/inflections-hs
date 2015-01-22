module Text.Inflections.Parse.Types ( Word(..), mapWord ) where

-- | A 'String' that should be kept whole through applied inflections
data Word

    -- | A word that may be transformed by inflection
    = Word String

    -- | A word that may not be transformed by inflections
    | Acronym String

    deriving (Show, Eq)

mapWord :: (String -> String) -> Word -> Word
mapWord f (Word s) = Word $ f s
mapWord f (Acronym s) = Acronym $ f s
