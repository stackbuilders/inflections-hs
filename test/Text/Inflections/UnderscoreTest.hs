module Text.Inflections.UnderscoreTest where

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

import Text.Inflections (underscore)

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: [Test]
tests = [testGroup "parsing"
          [ testCase "lower-camel case" test_lowerCamelCase
          , testCase "upper-camel case" test_upperCamelCase
          , testCase "invalid camel case" test_failedUnderscore
          ]
        ]

fromRight :: Either a b -> b
fromRight (Left _)  =
    error "Either.Unwrap.fromRight: Argument takes form 'Left _'"
fromRight (Right x) = x

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

test_lowerCamelCase = fromRight (underscore "testThis") @?= "test_this"
test_upperCamelCase = fromRight (underscore "TestThis") @?= "test_this"

test_failedUnderscore = True @?= (isLeft $ underscore "hey there")
