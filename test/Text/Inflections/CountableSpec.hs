{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.CountableSpec (spec) where

import Test.Hspec

import Text.Inflections (pluralize, pluralizeWith, singularize, singularizeWith)

spec :: Spec
spec =
  countableCases


countableCases :: Spec
countableCases = do

  let singlePlural = [
        ("person", "people"),
        ("ox", "oxen"),
        ("quiz", "quizes"),
        ("tomato", "tomatoes"),
        ("octopus", "octopi")]

  it "takes a custom mapping" $
    pluralizeWith singlePlural "quiz" `shouldBe` "quizes"

  it "can singularize irregulars" $
    singularize "people" `shouldBe` "person"

  it "returns itself when applied to both pluralize and singularize" $
    mapM_ equality (map fst singlePlural)
      where equality a = a `shouldBe` (singularize . pluralize) a

