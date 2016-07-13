{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.OrdinalSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck.Property
import qualified Data.Text as T

import Text.Inflections (ordinal, ordinalize)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

spec :: Spec
spec = do
  describe "ordinal" $ do
    it "returns the ordinal for 1" $
      ordinal (1 :: Integer) `shouldBe` "st"
    it "returns the ordinal for 2" $
      ordinal (2 :: Integer) `shouldBe` "nd"
    it "returns the ordinal for 1002" $
      ordinal (1002 :: Integer) `shouldBe` "nd"
    it "returns the ordinal for 1003" $
      ordinal (1003 :: Integer) `shouldBe` "rd"
    it "returns the ordinal for -11" $
      ordinal (-11 :: Integer) `shouldBe` "th"
    it "returns the ordinal for -1021" $
      ordinal (-1021 :: Integer) `shouldBe` "st"
    it "never returns empty output" $ property $
      property <$> not . T.null . (ordinal :: Integer -> Text)
  describe "ordinalize" $ do
    it "returns the full ordinal for 1" $
      ordinalize (1 :: Integer) `shouldBe` "1st"
    it "returns the full ordinal for -1021" $
      ordinalize (-1021 :: Integer) `shouldBe` "-1021st"
  it "always returns the number as prefix of the result" $
    property $ \n ->
      let s = show (n :: Integer)
      in T.pack s == T.take (length s) (ordinalize n)
