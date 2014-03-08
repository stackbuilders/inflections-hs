module Text.Inflections.TitleizeTest where

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

import Text.Inflections (titleize)

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: [Test]
tests = [ testGroup "titleize"
          [ testCase "employee_salary -> Employee Salary" test_titleize1
          , testCase "underground -> Underground" test_titleize2
          ]
        ]

----------------------------------------------------

fromRight :: Either a b -> b
fromRight (Left _)  =
    error "Either.Unwrap.fromRight: Argument takes form 'Left _'"
fromRight (Right x) = x

test_titleize1 = "Employee Salary" @?= fromRight (titleize "employee_salary")
test_titleize2 = "Underground" @?= fromRight (titleize "underground")
