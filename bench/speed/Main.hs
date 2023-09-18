{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Criterion.Main

import           Prelude          hiding (readFile, take)

import           Data.Text        (Text, pack, take)
import           Data.Text.IO     (readFile)

import           Text.Inflections (toCamelCased, toDashed, toHumanized,
                                   toUnderscore)


main :: IO ()
main = do
  text <- readFile "bench/little_women.txt"
  let !t1 = take 100 text
      !t2 = take 10000 text
      !t3 = take 1000000 text
  defaultMain
    [ bgroup "toUnderscore"
      [ bench "text-len-100" $ nf toUnderscore t1
      , bench "text-len-10000" $ nf toUnderscore t2
      , bench "text-len-1000000" $ nf toUnderscore t3
      , bench "all text" $ nf toUnderscore text
      ]
    , bgroup "toDashed"
      [ bench "text-len-100" $ nf toDashed t1
      , bench "text-len-10000" $ nf toDashed t2
      , bench "text-len-1000000" $ nf toDashed t3
      , bench "all text" $ nf toDashed text
      ]
    , bgroup "toCamelCased False"
      [ bench "text-len-100" $ nf (toCamelCased False) t1
      , bench "text-len-10000" $ nf (toCamelCased False) t2
      , bench "text-len-1000000" $ nf (toCamelCased False) t3
      , bench "all text" $ nf (toCamelCased False) text
      ]
    , bgroup "toHumanized False"
      [ bench "text-len-100" $ nf (toHumanized False) t1
      , bench "text-len-10000" $ nf (toHumanized False) t2
      , bench "text-len-1000000" $ nf (toHumanized False) t3
      , bench "all text" $ nf (toHumanized False) text
      ]
    ]
