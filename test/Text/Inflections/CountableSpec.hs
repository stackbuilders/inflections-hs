{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.CountableSpec (spec) where

import Test.Hspec

import Text.Inflections (pluralize, singularize)

spec :: Spec
spec = do
  -- pluralizeCases
  singularizeCases
  countableCases

pluralizeCases :: Spec
pluralizeCases = undefined

singularizeCases :: Spec
singularizeCases =
  it "can singularize irregulars" $
    singularize "people" `shouldBe` "person"

countableCases :: Spec
countableCases =

  it "returns itself when applied to both pluralize and singularize" $
    let singlePlural = [
         ("person", "people"),
         ("ox", "oxen"),
         ("quiz", "quizes"),
         ("tomato", "tomatoes"),
         ("octopus", "octopi")]

    in
      mapM_ equality (map fst singlePlural)
        where equality a = a `shouldBe` (singularize . pluralize) a

