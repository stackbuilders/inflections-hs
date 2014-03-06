module Text.Inflections.OrdinalTest where

import Test.HUnit hiding (Test)

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Test.Framework (Test, testGroup)

import Data.List (all, group)
import Data.Char (toLower)
import Data.Map (fromList)
import Text.Parsec
import Data.Maybe (fromJust)

import Text.Inflections (ordinal, ordinalize)

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: [Test]
tests = [ testGroup "ordinal"
          [ testCase "1 -> st" test_ordinal1
          , testCase "2 -> nd" test_ordinal2
          , testCase "1002 -> nd" test_ordinal1002
          , testCase "1003 -> rd" test_ordinal1003
          , testCase "-11 -> th" test_ordinalNegative11
          , testCase "-1021 -> st" test_ordinalNegative1021
          , testProperty "notEmpty" prop_ordinalReturnsNotEmpty
          ]
        , testGroup "ordinalize"
          [ testCase "1 -> st" test_ordinalize1
          , testCase "-1021 -> st" test_ordinalizeNegative1021
          , testProperty "result contains number" prop_ordinalizeSamePrefix
          ]
        ]

----------------------------------------------------

test_ordinal1 = "st" @?= ordinal 1

test_ordinal2 = "nd" @?= ordinal 2

test_ordinal1002 = "nd" @?= ordinal 1002

test_ordinal1003 = "rd" @?= ordinal 1003

test_ordinalNegative11 = "th" @?= ordinal (-11)

test_ordinalNegative1021 = "st" @?= ordinal (-1021)

----------------------------------------------------

test_ordinalize1 = "1st" @?= ordinalize 1

test_ordinalizeNegative1021 = "-1021st" @?= ordinalize (-1021)

----------------------------------------------------

prop_ordinalReturnsNotEmpty :: Integer -> Bool
prop_ordinalReturnsNotEmpty = not . null . ordinal

prop_ordinalizeSamePrefix :: Integer -> Bool
prop_ordinalizeSamePrefix n = show n == take (length $ show n) (ordinalize n)

