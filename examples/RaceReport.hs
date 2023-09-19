{-# LANGUAGE OverloadedStrings #-}

module RaceReport where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Text.Inflections as TI

{-
   In this example, we're using Inflections to present the results of a tournament
   in the way they arrive in a race.
-}

bikers :: [T.Text]
bikers =
  [ "Richard Carapaz"
  , "Juan Carlos Gaviria"
  , "María Fernanda López"
  , "Andrés Mendoza"
  , "Gabriela Vargas"
  , "Luis Eduardo Torres"
  ]

reportResult :: (T.Text, Int) -> T.Text
reportResult (name, pos) = TI.ordinalize pos <> " " <> name <> "\n"

reportResults :: T.Text
reportResults = mconcat $ zipWith (curry reportResult) bikers [1..]


{-
   >>> :set -package inflections
   >>> :load RaceReport.hs
   >>> main
   Race Results:
   1st Richard Carapaz
   2nd Juan Carlos Gaviria
   3rd María Fernanda López
   4th Andrés Mendoza
   5th Gabriela Vargas
   6td Luis Eduardo Torres

-}

main :: IO ()
main = do
  T.putStrLn "Race Results:"
  T.putStrLn reportResults
