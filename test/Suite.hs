module Main where

import  Test.Framework (defaultMain)

import qualified Text.Inflections.Tests
import qualified Text.Inflections.Parse.CamelCaseTest

main :: IO ()
main = defaultMain $ Text.Inflections.Tests.tests ++
                     Text.Inflections.Parse.CamelCaseTest.tests
