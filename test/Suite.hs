module Main where

import  Test.Framework (defaultMain)

import qualified Text.Inflections.Tests

main :: IO ()
main = defaultMain Text.Inflections.Tests.tests
