{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Weigh

import Data.Text (Text, pack)

import Text.Inflections
  ( toUnderscore
  )


main :: IO ()
main = mainWith $ do
  let textLen :: Int -> Text
      textLen n = pack (take n ['0'..])
  func "toUnderscore empty" toUnderscore mempty
  func "toUnderscore text-len-10" toUnderscore (textLen 10)
  func "toUnderscore text-len-100" toUnderscore (textLen 100)
  func "toUnderscore text-len-1000" toUnderscore (textLen 1000)
