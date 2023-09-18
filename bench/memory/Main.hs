{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Weigh

import           Prelude          hiding (readFile, take)

import           Data.Text        (Text, pack, take)
import           Data.Text.IO     (readFile)


import           Text.Inflections (toCamelCased, toDashed, toHumanized,
                                   toUnderscore)


main :: IO ()
main = do
  text <- readFile "bench/little_women.txt"
  mainWith $ do
    let !t1 = take 100 text
        !t2 = take 10000 text
        !t3 = take 1000000 text

    func "toUnderscore text-len-100" toUnderscore t1
    func "toUnderscore text-len-10000" toUnderscore t2
    func "toUnderscore text-len-1000000" toUnderscore t3
    func "toUnderscore all text" toUnderscore text

    func "toDashed text-len-100" toDashed t1
    func "toDashed text-len-10000" toDashed t2
    func "toDashed text-len-1000000" toDashed t3
    func "toDashed all text" toDashed text

    func "toCamelCased False text-len-100" (toCamelCased False) t1
    func "toCamelCased False text-len-10000" (toCamelCased False) t2
    func "toCamelCased False text-len-1000000" (toCamelCased False) t3
    func "toCamelCased False all text" (toCamelCased False) text

    func "toHumanized False text-len-100" (toHumanized False) t1
    func "toHumanized False text-len-10000" (toHumanized False) t2
    func "toHumanized False text-len-1000000" (toHumanized False) t3
    func "toHumanized False all text" (toHumanized False) text
