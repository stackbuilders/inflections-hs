module Text.Inflections.Parse.CamelCaseTest where

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

import Text.Inflections.Parse.CamelCase

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: [Test]
tests = [testGroup "parsing"
          [ testCase "lower-camel case" test_lowerCamelCase
          , testCase "upper-camel case" test_upperCamelCase
          ]
        ]

fromRight :: Either a b -> b
fromRight (Left _)  = error
                      "Either.Unwrap.fromRight: Argument takes form 'Left _'"
fromRight (Right x) = x

test_lowerCamelCase = fromRight (parse (parser []) "" "testThis") @?=
                      [Word "test", Word "This"]

test_upperCamelCase = fromRight (parse (parser []) "" "TestThis") @?=
                      [Word "Test", Word "This"]
