module Main where

import  Test.Framework (defaultMain)

import qualified Text.Inflections.Tests
import qualified Text.Inflections.UnderscoreTest
import qualified Text.Inflections.OrdinalTest
import qualified Text.Inflections.HumanizeTest
import qualified Text.Inflections.TitleizeTest

main :: IO ()
main = defaultMain $ Text.Inflections.Tests.tests ++
                     Text.Inflections.UnderscoreTest.tests ++
                     Text.Inflections.OrdinalTest.tests ++
                     Text.Inflections.HumanizeTest.tests ++
                     Text.Inflections.TitleizeTest.tests
