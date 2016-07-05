{-# LANGUAGE OverloadedStrings #-}

module Text.InflectionsSpec (spec) where

import Test.Hspec
import Text.Inflections (toUnderscore, toDashed, toCamelCased)

spec :: Spec
spec = do
  camelCaseToSnakeCase
  camelCaseToDashed
  snakeCaseToCamelCase

camelCaseToSnakeCase :: Spec
camelCaseToSnakeCase =
  it "converts camel case snake case" $
    toUnderscore "camelCasedText" `shouldReturn` "camel_cased_text"

camelCaseToDashed :: Spec
camelCaseToDashed =
  it "converts camel case to dashed" $
    toDashed "camelCasedText" `shouldReturn` "camel-cased-text"

snakeCaseToCamelCase :: Spec
snakeCaseToCamelCase =
  it "converts snake case to camel case" $
     toCamelCased False "underscored_text" `shouldReturn` "underscoredText"
