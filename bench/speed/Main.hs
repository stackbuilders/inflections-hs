{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Criterion.Main

import           Prelude          hiding (readFile, take)

import           Data.Text        (Text, pack, take)
import           Data.Text.IO     (readFile)

import           Text.Inflections (toUnderscore)


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
      ]
    ]
