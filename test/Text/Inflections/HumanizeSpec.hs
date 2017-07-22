{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.HumanizeSpec (spec) where

import Test.Hspec

import Text.Inflections

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

spec :: Spec
spec = do
  describe "humanize" $ do
    it "converts snake case to a human-readable string" $ do
      employee <- SomeWord <$> mkWord "employee"
      salary   <- SomeWord <$> mkWord "salary"
      humanize [employee,salary] `shouldBe` "Employee salary"
    it "turns underscores into spaces" $ do
      employee  <- SomeWord <$> mkWord "employee"
      has       <- SomeWord <$> mkWord "has"
      salary    <- SomeWord <$> mkWord "salary"
      humanize [employee, has, salary] `shouldBe` "Employee has salary"
    it "capitalizes the first word of a sentence" $ do
      underground <- SomeWord <$> mkWord "underground"
      humanize [underground] `shouldBe` "Underground"
  describe "humanizeCustom False" $ do
    it "converts snake case to a human-readable string" $ do
      employee <- SomeWord <$> mkWord "employee"
      salary   <- SomeWord <$> mkWord "salary"
      humanizeCustom False [employee,salary] `shouldBe` "employee salary"
    it "turns underscores into spaces" $ do
      employee  <- SomeWord <$> mkWord "employee"
      has       <- SomeWord <$> mkWord "has"
      salary    <- SomeWord <$> mkWord "salary"
      humanizeCustom False [employee, has, salary] `shouldBe` "employee has salary"
    it "lower-cases the first word of a sentence" $ do
      underground <- SomeWord <$> mkWord "underground"
      humanizeCustom False [underground] `shouldBe` "underground"
  describe "humanizeCustom True" $ do
    it "converts snake case to a human-readable string" $ do
      employee <- SomeWord <$> mkWord "employee"
      salary   <- SomeWord <$> mkWord "salary"
      humanizeCustom True [employee,salary] `shouldBe` "Employee salary"
    it "turns underscores into spaces" $ do
      employee  <- SomeWord <$> mkWord "employee"
      has       <- SomeWord <$> mkWord "has"
      salary    <- SomeWord <$> mkWord "salary"
      humanizeCustom True [employee, has, salary] `shouldBe` "Employee has salary"
    it "capitalizes the first word of a sentence" $ do
      underground <- SomeWord <$> mkWord "underground"
      humanizeCustom True [underground] `shouldBe` "Underground"
