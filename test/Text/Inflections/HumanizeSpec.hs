{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.HumanizeSpec (spec) where

import Test.Hspec

import Text.Inflections (humanize)
import Text.Inflections.Parse.Types (Word(..))

spec :: Spec
spec = describe "humazine" $ do
  it "converts snake case to a human-readable string" $
    humanize [Word "employee", Word "salary"] `shouldBe` "Employee salary"
  it "turns underscores into spaces" $
    humanize [Word "employee", Word "has_salary"] `shouldBe` "Employee has salary"
  it "capitalizes the first word of a sentence" $
    humanize [Word "underground"] `shouldBe` "Underground"
