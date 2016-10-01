{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.CountableSpec (spec) where

import Test.Hspec

import Text.Inflections

spec :: Spec
spec = do
  irregularCases
  matchingCases

matchingCases :: Spec
matchingCases = do
  it "pluralizes regex cases" $ do
    pluralize "ox" `shouldBe` "oxen"

  it "singularizes regex cases" $ do
    singularize "mice" `shouldBe` "mouse"

irregularCases :: Spec
irregularCases = do

  -- let singlePlural =
  --       fmap Simple
  --       [ ("person", "people")
  --       , ("ox", "oxen")
  --       , ("quiz", "quizes")
  --       , ("tomato", "tomatoes")
  --       , ("octopus", "octopi")
  --       ]

  -- it "takes a custom mapping" $
  --   pluralizeWith singlePlural "quiz" `shouldBe` "quizes"

  it "can singularize irregulars" $
    singularize "people" `shouldBe` "person"

  -- it "returns itself when applied to both pluralize and singularize" $
  --   mapM_ equality (map fst defaultIrregulars)
  --     where equality a = a `shouldBe` (singularize . pluralize) a
