{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.CountableSpec (spec) where

import Test.Hspec
import Data.Maybe

import Text.Inflections
import Text.Inflections.Countable

spec :: Spec
spec = do
  irregularCases
  matchingCases

matchingCases :: Spec
matchingCases = do
  it "replaces the non-grouped patterns" $ do
    let reg = fromJust $ regexPattern "s$"
    fromJust (matchWithReplace (reg, "") "things") `shouldBe` "thing"

  it "replaces the grouped patterns" $ do
    let reg = fromJust $ regexPattern "(ch)es$"
    fromJust (matchWithReplace (reg, "\1") "branches") `shouldBe` "branch"

    let reg2 = fromJust $ regexPattern "^(ox)$"
    fromJust (matchWithReplace (reg2, "\1en") "ox") `shouldBe` "oxen"

  it "handles the normal case" $ do
    pluralize "thing" `shouldBe` "things"

  it "pluralizes regex cases" $ do
    pluralize "ox" `shouldBe` "oxen"

  it "singularizes regex cases" $ do
    singularize "mice" `shouldBe` "mouse"
    singularize "oxen" `shouldBe` "ox"
    singularize "branches" `shouldBe` "branch"

  -- TODO stop lazy eval?
  it "returns itself when applied to both pluralize and singularize" $
    mapM_ equality ["thing", "branch", "ox"]
      where equality a = a `shouldBe` (singularize . pluralize) a

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
