{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.TransliterateSpec
  ( spec )
where

import Test.Hspec
import Text.Inflections (transliterate)

spec :: Spec
spec = describe "transliterate" $ do
   withSubstitutions
   withoutSubstitutions
   missingSubstitutions

withoutSubstitutions :: Spec
withoutSubstitutions =
  it "transliterates without subsitutions" $
     transliterate "this is a test" `shouldBe` "this is a test"

withSubstitutions :: Spec
withSubstitutions =
  it "transliterates with substitution" $
     transliterate "Feliz año nuevo" `shouldBe` "Feliz ano nuevo"

missingSubstitutions :: Spec
missingSubstitutions =
  it "transliterates with missing substitutions" $
    transliterate "Have a ❤ ñ!" `shouldBe` "Have a ? n!"
