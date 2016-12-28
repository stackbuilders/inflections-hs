{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Parse.SnakeCaseSpec
  ( spec )
where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Inflections

spec :: Spec
spec =
  describe "parseSnakeCase" $ do
    context "when given no acronyms" $ do
      it "parses snake_case correctly" $ do
        r <- mapM (fmap SomeWord . mkWord) ["OneTwo","Three"]
        parseSnakeCase [] "OneTwo_Three" `shouldParse` r
      it "handles digits in the words correctly" $ do
        r <- mapM (fmap SomeWord . mkWord) ["one4a","two00"]
        parseSnakeCase [] "one4a_two00" `shouldParse` r
    context "when given some acronyms" $ do
      it "parses snake_case correctly" $ do
        a <- mkAcronym "BOO"
        r <- mapM (fmap SomeWord . mkWord) ["BOO","one","one"]
        parseSnakeCase [a] "BOO_one_one" `shouldParse` r
      context "when acronym happens to be a prefix of other word" $
        it "parses the word correctly" $ do
          a <- mkAcronym "BOO"
          r <- mapM (fmap SomeWord . mkWord) ["one","BOOtwo"]
          parseSnakeCase [a] "one_BOOtwo" `shouldParse` r
      context "when acronym happens to be a suffix of other word" $
        it "parses the word correctly" $ do
          a <- mkAcronym "BOO"
          r <- mapM (fmap SomeWord . mkWord) ["oneBOO","two"]
          parseSnakeCase [a] "oneBOO_two" `shouldParse` r
