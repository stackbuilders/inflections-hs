{-# LANGUAGE OverloadedStrings #-}

module Text.InflectionsSpec (spec) where

import Data.Void
import qualified Data.Maybe as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Test.Hspec
import Test.QuickCheck
import Text.Inflections
import Text.Megaparsec

newtype WrapperParseError =
  WrapperParseError { unParseError :: ParseError Char Void }
    deriving (Show)

instance Arbitrary WrapperParseError where
  arbitrary = WrapperParseError <$> oneof [trivialError, fancyError]
    where
      trivialError = TrivialError <$> nonEmptyArbitrary <*> maybeErrorItem <*> setErrorItem
      fancyError = FancyError <$> nonEmptyArbitrary <*> setErrorFancy
      nonEmptyArbitrary = safeNonEmpty <$> listOf (unSourcePos <$> arbitrary)
      singleNonEmpty = NE.fromList [ SourcePos "" (mkPos 1) (mkPos 1) ]
      safeNonEmpty list = M.fromMaybe singleNonEmpty (NE.nonEmpty list)
      setErrorFancy = S.fromList <$> listOf (unErrorFancy <$> arbitrary)
      maybeErrorItem = oneof [ Just <$> (unErrorItem <$> arbitrary), return Nothing]
      setErrorItem = S.fromList <$> listOf (unErrorItem <$> arbitrary)

newtype WrapperSourcePos =
  WrapperSourcePos { unSourcePos :: SourcePos }
    deriving (Show)

instance Arbitrary WrapperSourcePos where
  arbitrary = WrapperSourcePos <$> (SourcePos <$> arbitrary <*> posArbitrary <*> posArbitrary)
    where
      posArbitrary = mkPos <$> arbitrary

newtype WrapperErrorFancy e =
  WrapperErrorFancy { unErrorFancy :: ErrorFancy e }
    deriving (Show)

instance Arbitrary (WrapperErrorFancy e) where
  arbitrary = WrapperErrorFancy <$> oneof [ ErrorFail <$> arbitrary ]

newtype WrapperErrorItem t =
  WrapperErrorItem { unErrorItem :: ErrorItem t }
    deriving (Show)

instance (Arbitrary t) => Arbitrary (WrapperErrorItem t) where
  arbitrary = WrapperErrorItem <$> oneof [ tokens_, labels_, return EndOfInput ]
    where
      tokens_ = Tokens <$> (NE.fromList <$> arbitrary)
      labels_ = Label <$> (NE.fromList <$> arbitrary)

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
        property $ \err ->
          betterThrow (Left (unParseError err)) `shouldThrow`
            (== InflectionParsingFailed (unParseError err))

    context "when given a value in Right" $
      it "returns the value" $
        property $ \x ->
          betterThrow (Right x) `shouldReturn` (x :: Int)
