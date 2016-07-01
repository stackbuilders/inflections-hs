{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.UnderscoreSpec (spec) where

import Test.Hspec
import Text.Inflections (underscore)
import Text.Inflections.Parse.Types (Word(..))

spec :: Spec
spec =
  it "converts a word list to snake case" $
     underscore [Word "test", Word "this"] `shouldBe` "test_this"
