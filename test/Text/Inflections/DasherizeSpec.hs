{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.DasherizeSpec
  ( spec )
where

import Test.Hspec

import Text.Inflections (dasherize)
import Text.Inflections.Parse.Types (Word (..))

spec :: Spec
spec = describe "dasherize" $
  it "dasherizes a collection of words sentence" $
    dasherize [Word "foo", Word "bar"] `shouldBe` "foo-bar"
