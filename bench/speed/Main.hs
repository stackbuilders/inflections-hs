{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main

import Data.Text (Text, pack)

import Text.Inflections
  ( toUnderscore
  )


main :: IO ()
main =
  defaultMain
    [ bgroup "toUnderscore"
      [ bench "mempty" $ nf toUnderscore mempty
      , bench "text-len-10" $ nf toUnderscore (textLen 10)
      , bench "text-len-100" $ nf toUnderscore (textLen 100)
      , bench "text-len-1000" $ nf toUnderscore (textLen 1000)
      ]
    ]
  where textLen :: Int -> Text
        textLen n = pack (take n ['0'..])
