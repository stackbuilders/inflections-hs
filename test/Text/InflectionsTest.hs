module Text.InflectionsTest where

import Test.HUnit hiding (Test)

import Test.Framework.Providers.HUnit (testCase)

import Test.Framework (Test, testGroup)

import Text.Inflections (toUnderscore, toDashed, toCamelCased)

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
