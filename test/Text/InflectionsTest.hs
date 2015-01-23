module Text.InflectionsTest where

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

import Text.Inflections (toUnderscore, toDashed, toCamelCased)
import Text.Inflections.Parse.Types (Word(..))

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: [Test]
tests = [ testGroup "toUnderscore"
          [ testCase "camelCasedText -> camel_cased_text" test_to_underscore
          ]
        , testGroup "toDashed"
          [ testCase "camelCasedText -> camel-cased-text" test_to_dashed
          ]
        , testGroup "toCamelCased"
          [ testCase "underscored_text -> camelCasedText" test_to_camel_cased
          ]
        ]

test_to_underscore = "camel_cased_text" @?= toUnderscore "camelCasedText"
test_to_dashed = "camel-cased-text" @?= toDashed "camelCasedText"
test_to_camel_cased = "underscoredText" @?= toCamelCased False "underscored_text"
