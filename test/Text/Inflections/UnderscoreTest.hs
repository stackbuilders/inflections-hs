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
import Text.Inflections.Parse.Types (Word(..))

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: [Test]
tests = [testGroup "underscore"
          [ testCase "testThis -> test_this" test_underscore
          ]
        ]

test_underscore = "test_this" @?= (underscore [Word "test", Word "this"])
