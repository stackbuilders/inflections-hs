module Text.Inflections.Tests where

import Test.HUnit hiding (Test)

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck

import Test.Framework (Test, testGroup)

import Data.List (group)
import Data.Char (toLower)

import Text.Inflections
import Text.Inflections.Parse.Types (Word(..))

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: [Test]
tests = [testGroup "dasherize"
         [ testCase "foo bar -> foo-bar" test_dasherize1
         ],

         testGroup "transliterate"
         [ testCase "Without substitutions" test_correctTransliterationWithoutSubs
         , testCase "With substitutions" test_correctTransliterationWithSubs
         , testCase "Missing subs" test_correctTransliterationMissingSubs
         ],

         testGroup "parameterize"
         [ testProperty "Contains only valid chars"
                        prop_parameterize1
         , testProperty "Does not begin with a separator character"
                         prop_parameterize2
         , testProperty "Does not end in a separator character"
                         prop_parameterize3
         , testProperty "All alphanumerics in input exist in output"
                        prop_parameterize4
         , testProperty "Doesn't have subsequences of more than one hyphen"
                        prop_parameterize5
         ]
        ]


test_correctTransliterationWithoutSubs =
    transliterate "this is a test" @?= "this is a test"

test_correctTransliterationWithSubs =
    transliterate "Feliz año nuevo" @?= "Feliz ano nuevo"

test_correctTransliterationMissingSubs =
    transliterate "Have a ❤ ñ!" @?= "Have a ? n!"

fromRight :: Either a b -> b
fromRight (Left _)  =
    error "Either.Unwrap.fromRight: Argument takes form 'Left _'"
fromRight (Right x) = x

isRight :: Either a b -> Bool
isRight (Left _)  = False
isRight (Right _) = True


test_dasherize1 = "foo-bar" @?= dasherize [Word "foo", Word "bar"]

prop_parameterize1 :: String -> Bool
prop_parameterize1 sf = all (`elem` (alphaNumerics ++ "-_")) $
                        parameterize sf

prop_parameterize2 :: String -> Property
prop_parameterize2 s =
    (not . null) parameterized ==> head parameterized /= '-'
    where parameterized = parameterize s

prop_parameterize3 :: String -> Property
prop_parameterize3 s =
    (not . null) parameterized ==> last parameterized /= '-'
    where parameterized = parameterize s

prop_parameterize4 :: String -> Bool
prop_parameterize4 s = all (\c -> c `notElem` alphaNumerics ||
                              c `elem` (alphaNumerics ++ "-") &&
                              c `elem` parameterized) $ map toLower s
    where parameterized = parameterize s

prop_parameterize5 :: String -> Bool
prop_parameterize5 s = longestSequenceOf '-' parameterized <= 1
    where parameterized = parameterize s


-- Helper functions and shared tests

longestSequenceOf :: Char -> String -> Int
longestSequenceOf c [] = 0
longestSequenceOf c s =
    if null subseqLengths then 0 else maximum subseqLengths

  where subseqLengths = (map length . filter (\str -> head str == c) . group) s

numMatching char str = length $ filter (== char) str

alphaNumerics :: String
alphaNumerics = ['a'..'z'] ++ ['0'..'9']
