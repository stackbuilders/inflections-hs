module Text.Inflections.HumanizeTest where

import Test.HUnit hiding (Test)

import Test.Framework.Providers.HUnit (testCase)

import Test.Framework (Test, testGroup)

import Text.Inflections (humanize)
import Text.Inflections.Parse.Types (Word(..))

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: [Test]
tests = [ testGroup "humanize"
          [ testCase "employee_salary -> Employee salary" test_humanize1
          , testCase "underground -> underground" test_humanize2
          ]
        ]

----------------------------------------------------

test_humanize1 = "Employee salary" @?=
                 humanize [Word "employee", Word "salary"]

test_humanize2 = "Underground" @?= humanize [Word "underground"]
