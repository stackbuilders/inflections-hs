{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.HumanizeSpec (spec) where

import Test.Hspec

import Text.Inflections (humanize)
import Text.Inflections.Types

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

spec :: Spec
spec = describe "humazine" $ do
  it "converts snake case to a human-readable string" $ do
    employee <- SomeWord <$> mkWord "employee"
    salary   <- SomeWord <$> mkWord "salary"
    humanize [employee,salary] `shouldBe` "Employee salary"
  it "turns underscores into spaces" $ do
    employee  <- SomeWord <$> mkWord "employee"
    hasSalary <- SomeWord <$> mkWord "has_salary"
    humanize [employee, hasSalary] `shouldBe` "Employee has salary"
  it "capitalizes the first word of a sentence" $ do
    underground <- SomeWord <$> mkWord "underground"
    humanize [underground] `shouldBe` "Underground"
