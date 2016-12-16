{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.TitleizeSpec (spec) where

import Test.Hspec
import Text.Inflections (titleize)
import Text.Inflections.Types

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

spec :: Spec
spec = describe "titleize" $ do
  it "converts two words to title case" $ do
    employee <- SomeWord <$> mkWord "Employee"
    salary   <- SomeWord <$> mkWord "Salary"
    titleize [employee,salary] `shouldBe` "Employee Salary"
  it "converts one word to title case" $ do
    underground <- SomeWord <$> mkWord "underground"
    titleize [underground] `shouldBe` "Underground"
