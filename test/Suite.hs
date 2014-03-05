module Main where

import  Test.Framework (defaultMain)

import qualified Text.Inflections.Tests
import qualified Text.Inflections.UnderscoreTest
import qualified Text.Inflections.OrdinalTest

main :: IO ()
main = defaultMain $ Text.Inflections.Tests.tests ++
                     Text.Inflections.UnderscoreTest.tests ++
                     Text.Inflections.OrdinalTest.tests
