{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.PropertiesSpec (spec) where

import Data.Char (toLower)
import Data.List (group)
import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import Text.Inflections
import Text.Inflections.Parse.Types (Word(..))

spec :: Spec
spec = do
  describe "dasherize"
    dasherizeSpacedSentence
  describe "transliterate" $ do
    withSubstitutions
    withoutSubstitutions
    missingSubstitutions
  describe "parameterize" $ do
    onlyValidCharacters
    notBeginWithSeparator
    notEndWithSeparator
    noMissingAlphanumerics
    noMoreThanOneHyphen

withoutSubstitutions :: Spec
withoutSubstitutions =
  it "transliterates without subsitutions" $
     transliterate "this is a test" `shouldBe` "this is a test"

withSubstitutions :: Spec
withSubstitutions =
  it "transliterates with substitution" $
     transliterate "Feliz año nuevo" `shouldBe` "Feliz ano nuevo"

missingSubstitutions :: Spec
missingSubstitutions =
  it "transliterates with missing substitutions" $
    transliterate "Have a ❤ ñ!" `shouldBe` "Have a ? n!"

dasherizeSpacedSentence :: Spec
dasherizeSpacedSentence =
  it "dasherizes a space separated sentence" $
    dasherize [Word "foo", Word "bar"] `shouldBe` "foo-bar"

onlyValidCharacters :: Spec
onlyValidCharacters =
  it "returns only valid characters" (property onlyValidCharactersPredicate)
    where onlyValidCharactersPredicate sf
            = T.all (`elem` (alphaNumerics ++ "-_")) $ parameterize (T.pack sf)

notBeginWithSeparator :: Spec
notBeginWithSeparator =
  it "never returns a string beginning ending with a separator" (property notBeginWithSeparatorPredicate)
      where
        notBeginWithSeparatorPredicate s =
            let parameterized = parameterize (T.pack s) in
            (not . T.null) parameterized ==> T.head parameterized /= '-'

notEndWithSeparator :: Spec
notEndWithSeparator =
  it "never returns a string beginning with a separator" (property notBeginWithSeparatorPredicate)
     where
       notBeginWithSeparatorPredicate s =
            let parameterized = parameterize (T.pack s) in
            (not . T.null) parameterized ==> T.last parameterized /= '-'

noMissingAlphanumerics :: Spec
noMissingAlphanumerics =
  it "returns every alphanumeric character from the input" (property noMissingAlphanumericsPredicate)
  where noMissingAlphanumericsPredicate s =
            let parameterized = parameterize (T.pack s) in
            all (\c -> c `notElem` alphaNumerics ||
                c `elem` (alphaNumerics ++ "-") &&
                c `elem` T.unpack parameterized) $ map toLower s

noMoreThanOneHyphen :: Spec
noMoreThanOneHyphen =
  it "never returns a string with a sequence of dashes" (property noMoreThanOneHyphenPredicate)
  where noMoreThanOneHyphenPredicate s =
            let parameterized = parameterize (T.pack s)
            in longestSequenceOf '-' (T.unpack parameterized) <= 1

longestSequenceOf :: Char -> String -> Int
longestSequenceOf _ [] = 0
longestSequenceOf c s =
    if null subseqLengths then 0 else maximum subseqLengths
  where subseqLengths = (map length . filter (\str -> head str == c) . group) s

alphaNumerics :: String
alphaNumerics = ['a'..'z'] ++ ['0'..'9']
