{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.DasherizeSpec
  ( spec )
where

import Test.Hspec

import Text.Inflections (dasherize)
import Text.Inflections.Types

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

spec :: Spec
spec = describe "dasherize" $
  it "dasherizes a collection of words sentence" $ do
    foo <- SomeWord <$> mkWord "foo"
    bar <- SomeWord <$> mkWord "bar"
    dasherize [foo,bar] `shouldBe` "foo-bar"
