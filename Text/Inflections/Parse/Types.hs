module Text.Inflections.Parse.Types ( Word(..) ) where

data Word = Word String
          | Acronym String deriving (Show, Eq)

