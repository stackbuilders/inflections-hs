{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Parse.CamelCaseSpec
  ( spec )
where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Inflections

spec :: Spec
spec =
  describe "parseCamelCase" $ do
    context "when given no acronyms" $ do
      context "when first word is capitalized" $
        it "parses CamelCase correctly" $ do
          r <- mapM (fmap SomeWord . mkWord) ["One","Two","Three"]
          parseCamelCase [] "OneTwoThree" `shouldParse` r
      context "when first word is not capitalized" $
        it "parses camelCase correctly" $ do
          r <- mapM (fmap SomeWord . mkWord) ["one","Two","Three"]
          parseCamelCase [] "oneTwoThree" `shouldParse` r
      context "when there are digits in the words" $
        it "parses CamelCase correctly" $ do
          r <- mapM (fmap SomeWord . mkWord) ["one1two","Three3"]
          parseCamelCase [] "one1twoThree3" `shouldParse` r
    context "when given some acronyms" $ do
      context "when first word is capitalized" $
        it "parses CamelCase correctly" $ do
          a <- mkAcronym "BOO"
          r <- mapM (fmap SomeWord . mkWord) ["One","BOO","One"]
          parseCamelCase [a] "OneBOOOne" `shouldParse` r
      context "when first word is not capitalized" $
        it "parses camelCase correctly" $ do
          a <- mkAcronym "BOO"
          r <- mapM (fmap SomeWord . mkWord) ["one","Two","BOO"]
          parseCamelCase [a] "oneTwoBOO" `shouldParse` r
      context "when there are digits in the words" $
        it "parses CamelCase correctly" $ do
          a <- mkAcronym "BOO"
          r <- mapM (fmap SomeWord . mkWord) ["one1two","Three3"]
          parseCamelCase [a] "one1twoThree3" `shouldParse` r
      context "when a word has a suffix coinciding with a acronym" $
        it "is still parsed correctly as a normala word" $ do
          a <- mkAcronym "boo"
          r <- mapM (fmap SomeWord . mkWord) ["fooboo","Bar"]
          parseCamelCase [a] "foobooBar" `shouldParse` r
