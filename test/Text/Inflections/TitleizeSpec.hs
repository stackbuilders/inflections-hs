module Text.Inflections.TitleizeSpec (spec) where

import Test.Hspec

import Text.Inflections (titleize)
import Text.Inflections.Parse.Types (Word(..))

spec :: Spec
spec = do
  twoWordsToTitleCase
  oneWordToTitleCase

twoWordsToTitleCase :: Spec
twoWordsToTitleCase =
  it "converts two words to title case" $
    titleize [Word "Employee", Word "Salary"] `shouldBe` "Employee Salary"

oneWordToTitleCase :: Spec
oneWordToTitleCase =
  it "converts one word to title case" $
     titleize [Word "underground"] `shouldBe` "Underground"
