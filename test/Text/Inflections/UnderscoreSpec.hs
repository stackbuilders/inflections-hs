{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.UnderscoreSpec (spec) where

import Test.Hspec
import Text.Inflections

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

spec :: Spec
spec = describe "underscore" $
  it "converts a word list to snake case" $ do
    test <- SomeWord <$> mkWord "test"
    this <- SomeWord <$> mkWord "this"
    underscore [test, this] `shouldBe` "test_this"
