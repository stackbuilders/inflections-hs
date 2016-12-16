{-# LANGUAGE OverloadedStrings #-}

module Text.InflectionsSpec (spec) where

import Test.Hspec
import Text.Inflections (toUnderscore, toDashed, toCamelCased)

spec :: Spec
spec = do
  describe "toUnderscore" $ do
    it "converts camel case to snake case" $
      toUnderscore "camelCasedText" `shouldReturn` "camel_cased_text"
    it "converts camel case to snake case with numbers" $
      toUnderscore "ipv4Address" `shouldReturn` "ipv4_address"
  describe "toDashed" $
    it "converts camel case to dashed" $
      toDashed "camelCasedText" `shouldReturn` "camel-cased-text"
  describe "toCamelCased" $ do
    context "when the first argument is False" $
      it "converts snake case to camel case" $
        toCamelCased False "underscored_text" `shouldReturn` "underscoredText"
    context "when the first argument is True" $
      it "converts snake case to camel case" $
        toCamelCased True "underscored_text" `shouldReturn` "UnderscoredText"
