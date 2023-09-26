{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Criterion.Main

import           Prelude          hiding (readFile, take, words)

import           Data.Maybe (catMaybes)

import           Data.Text        (Text, pack, take, words)
import           Data.Text.IO     (readFile)

import           Text.Inflections (SomeWord(..), mkWord, mkAcronym, ordinal, parameterize,
                                   titleize, toCamelCased, toDashed, toHumanized, toUnderscore)


main :: IO ()
main = do
  text <- readFile "bench/little_women.txt"
  let !t1 = take 100 text
      !t2 = take 10000 text
      !t3 = take 1000000 text

      somewords txt = catMaybes $ mkSomeWords $ words txt

      mkSomeWords (f:s:rest) = (SomeWord <$> mkWord f) : (SomeWord <$> mkAcronym s) : mkSomeWords rest
      mkSomeWords (f:rest) = (SomeWord <$> mkWord f) : mkSomeWords rest
      mkSomeWords [] = []

      benchTextToText name func =
        bgroup name
          [ bench "text-len-100" $ nf func t1
          , bench "text-len-10000" $ nf func t2
          , bench "text-len-1000000" $ nf func t3
          , bench "all text" $ nf func text
          ]

  defaultMain
    [ benchTextToText "toUnderscore" toUnderscore
    , benchTextToText "toDashed" toDashed
    , benchTextToText "toCamelCased False" (toCamelCased False)
    , benchTextToText "toHumanized False" (toHumanized False)
    , benchTextToText "parameterize" parameterize
    , bgroup "titleize"
        [ bench "text-len-100" $ nf titleize (somewords t1)
        , bench "text-len-10000" $ nf titleize (somewords t2)
        , bench "text-len-1000000" $ nf titleize (somewords t3)
        , bench "all text" $ nf titleize (somewords text)
        ]
    , bgroup "ordinal"
        [ bench "1" $ nf ordinal 1
        , bench "3" $ nf ordinal 13
        , bench "13" $ nf ordinal 13
        , bench "137" $ nf ordinal 137
        ]
    ]
