module Text.Inflections.HumanizeSpec (spec) where

import Test.Hspec

import Text.Inflections (humanize)
import Text.Inflections.Parse.Types (Word(..))

spec :: Spec
spec = do
  snakeCaseHumanize
  capitalizeFirstWord

snakeCaseHumanize :: Spec
snakeCaseHumanize =
  it "converts snake case to a human-readable string" $
    humanize [Word "employee", Word "salary"] `shouldBe` "Employee salary"

capitalizeFirstWord :: Spec
capitalizeFirstWord =
  it "capitalizes the first word of a sentence" $
    humanize [Word "underground"] `shouldBe` "Underground"
