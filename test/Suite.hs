module Main where

import  Test.Framework (defaultMain)

import qualified Text.Inflections.Tests
import qualified Text.Inflections.UnderscoreTest

main :: IO ()
main = defaultMain $ Text.Inflections.Tests.tests ++
                     Text.Inflections.UnderscoreTest.tests
