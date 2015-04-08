module Text.Inflections.TitleizeTest where

import Test.HUnit hiding (Test)

import Test.Framework.Providers.HUnit (testCase)

import Test.Framework (Test, testGroup)

import Text.Inflections (titleize)
import Text.Inflections.Parse.Types (Word(..))

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: [Test]
tests = [ testGroup "titleize"
          [ testCase "employee_salary -> Employee Salary" test_titleize1
          , testCase "underground -> Underground" test_titleize2
          ]
        ]

----------------------------------------------------

test_titleize1 = "Employee Salary" @?=
                 titleize [Word "Employee", Word "Salary"]
test_titleize2 = "Underground" @?= titleize [Word "underground"]
