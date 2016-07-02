-- |
-- Module      :  Text.Inflections.Ordinal
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversion to spelled ordinal numbers.

module Text.Inflections.Ordinal (ordinal, ordinalize)
where

-- |Returns the suffix that should be added to a number to denote the position
-- in an ordered sequence such as 1st, 2nd, 3rd, 4th.
--
-- >>> ordinal 1
-- "st"
-- >>> ordinal 2
-- "nd"
-- >>> ordinal 10
-- "th"
ordinal :: Integral a => a -> String
ordinal number
        | remainder100 `elem` [11..13] = "th"
        | remainder10 == 1             = "st"
        | remainder10 == 2             = "nd"
        | remainder10 == 3             = "rd"
        | otherwise                    = "th"
  where abs_number   = abs number
        remainder10  = abs_number `mod` 10
        remainder100 = abs_number `mod` 100

-- |Turns a number into an ordinal string used to denote the position in an
-- ordered sequence such as 1st, 2nd, 3rd, 4th.
--
-- >>> ordinalize 1
-- "1st"
-- >>> ordinalize 2
-- "2nd"
-- >>> ordinalize 10
-- "10th"
ordinalize :: (Integral a, Show a) => a -> String
ordinalize n = show n ++ ordinal n
