module Text.Inflections.HumanizeTest where

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

import Text.Inflections (humanize)

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: [Test]
tests = [ testGroup "humanize"
          [ testCase "employee_salary -> Employee salary" test_humanize1
          , testCase "underground -> underground" test_humanize2
          ]
        ]

----------------------------------------------------

fromRight :: Either a b -> b
fromRight (Left _)  =
    error "Either.Unwrap.fromRight: Argument takes form 'Left _'"
fromRight (Right x) = x

test_humanize1 = "Employee salary" @?= fromRight (humanize "employee_salary")
test_humanize2 = "Underground" @?= fromRight (humanize "underground")
