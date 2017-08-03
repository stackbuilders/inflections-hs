{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.InflectionsSpec (spec) where

import Data.Void
import Test.Hspec
import Test.QuickCheck
import Text.Inflections
import Text.Megaparsec

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

arbitraryParseError :: Gen (ParseError Char Void)
arbitraryParseError = oneof [trivialError, fancyError]
    where
      trivialError = TrivialError <$> nonEmptyArbitrary <*> maybeErrorItem <*> setErrorItem
      fancyError = FancyError <$> nonEmptyArbitrary <*> setErrorFancy
      nonEmptyArbitrary = NE.fromList <$> listOf1 arbitrarySourcePos
      setErrorFancy = S.fromList <$> listOf arbitraryErrorFancy
      maybeErrorItem = oneof [ Just <$> arbitraryErrorItem, return Nothing]
      setErrorItem = S.fromList <$> listOf arbitraryErrorItem

arbitrarySourcePos :: Gen SourcePos
arbitrarySourcePos = SourcePos <$> arbitrary <*> posArbitrary <*> posArbitrary
    where
      posArbitrary = mkPos <$> ((+1) . abs <$> arbitrary)

arbitraryErrorFancy :: Gen (ErrorFancy e)
arbitraryErrorFancy = oneof [ ErrorFail <$> arbitrary ]

arbitraryErrorItem :: (Arbitrary e) => Gen (ErrorItem e)
arbitraryErrorItem = oneof [ tokens_, labels_, return EndOfInput ]
    where
      tokens_ = Tokens <$> (NE.fromList <$> listOf1 arbitrary)
      labels_ = Label <$> (NE.fromList <$> listOf1 arbitrary)

spec :: Spec
spec = do

  describe "toUnderscore" $ do
    it "converts camel case to snake case" $
      toUnderscore "camelCasedText" `shouldBe` Right "camel_cased_text"
    it "converts camel case to snake case with numbers" $
      toUnderscore "ipv4Address" `shouldBe` Right "ipv4_address"

  describe "toDashed" $
    it "converts camel case to dashed" $
      toDashed "camelCasedText" `shouldBe` Right "camel-cased-text"

  describe "toCamelCased" $ do
    context "when the first argument is False" $
      it "converts snake case to camel case" $
        toCamelCased False "underscored_text" `shouldBe` Right "underscoredText"
    context "when the first argument is True" $
      it "converts snake case to camel case with the first word capitalized" $
        toCamelCased True "underscored_text" `shouldBe` Right "UnderscoredText"

  describe "toHumanized" $ do
    context "when the first argument is False" $
      it "converts snake case to human-readable form with lower-case initial letter" $
        toHumanized False "underscored_text" `shouldBe` Right "underscored text"
    context "when the first argument is True" $
      it "converts snake case to human-readable form with the first word capitalized" $
        toHumanized True "underscored_text" `shouldBe` Right "Underscored text"

  describe "betterThrow" $ do
    context "when given a parse error" $
      it "throws the correct exception" $
        property $ forAll arbitraryParseError $ \err ->
          betterThrow (Left err) `shouldThrow`
            (== InflectionParsingFailed err)

    context "when given a value in Right" $
      it "returns the value" $
        property $ \x ->
          betterThrow (Right x) `shouldReturn` (x :: Int)
