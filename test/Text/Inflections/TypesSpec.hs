{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.TypesSpec
  ( spec )
where

import Test.Hspec
import Text.Inflections
import qualified Data.Text as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

spec :: Spec
spec = do
  describe "mkWord" $ do
    context "when provided a correct Text value" $
      it "creates a normal word" $ do
        x <- mkWord "foo"
        unWord x `shouldBe` "foo"
    context "when provided an incorrect Text value" $
      it "throws the correct exception" $ do
        mkWord "foo bar" `shouldThrow` (== InflectionInvalidWord "foo bar")
        mkWord "foo_" `shouldThrow` (== InflectionInvalidWord "foo_")
        mkWord "&$?%" `shouldThrow` (== InflectionInvalidWord "&$?%")
  describe "mkAcronym" $ do
    context "when provided a correct Text value" $
      it "creates an acronym" $ do
        x <- mkAcronym "foo"
        unWord x `shouldBe` "foo"
    context "when providde an incorrect Text value" $
      it "throws the correct exception" $ do
        mkAcronym "foo bar" `shouldThrow` (== InflectionInvalidAcronym "foo bar")
        mkAcronym "foo_" `shouldThrow` (== InflectionInvalidAcronym "foo_")
        mkAcronym "&$?%" `shouldThrow` (== InflectionInvalidAcronym "&$?%")
  describe "unWord" $
    it "extracts the inner Text value" $ do
      (unWord <$> mkWord "foo")    `shouldReturn` "foo"
      (unWord <$> mkWord "bar")    `shouldReturn` "bar"
      (unWord <$> mkAcronym "baz") `shouldReturn` "baz"
  describe "unSomeWord" $ do
    context "when inner value is a normal word" $
      it "Text is extracted and the given function applied" $ do
        x <- SomeWord <$> mkWord "word"
        unSomeWord T.toUpper x `shouldBe` "WORD"
    context "when inner value is an acronym" $
      it "Text is extracted, but the function is not applied" $ do
        x <- SomeWord <$> mkAcronym "acronym"
        unSomeWord T.toUpper x `shouldBe` "acronym"
