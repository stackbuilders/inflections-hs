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

reg (p, r) = matchWithReplace (pat, r)
  where
    pat = fromJust $ regexPattern p
matchingCases :: Spec
matchingCases = do
  describe "laws" $ do
    let equality1 a = a `shouldBe` (singularize . pluralize) a
    let equality2 a = a `shouldBe` (pluralize . singularize) a

    it "returns itself when applied to (singularize . pluralize)" $
      mapM_ equality1 ["thing", "branch", "ox"]

    it "returns itself when applied to (pluralize . singularize)" $
      mapM_ equality2 ["things", "branches", "oxen"]

  describe "matchWithReplace" $ do
    it "replaces the non-grouped patterns" $ do
      let in_n_out = [ (("s$", ""), ("things", "thing"))
                     , (("$", "s"), ("thing", "things"))
                     ]

      mapM_ (\(in', out) -> fromJust(reg in' (fst out)) `shouldBe` snd out ) in_n_out

    it "replaces the grouped patterns for singularize" $ do
      let in_n_out = [ (("(ch)es$", "\1"), ("branches", "branch"))
                     , (("^(ox)en", "\1"), ("oxen", "ox"))
                     , (("(octop|vir)us$", "\1i"), ("octopus", "octopi"))
                     , (("((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$", "\1sis"), ("analyses", "analysis"))
                     ]

      mapM_ (\(in', out) -> fromJust(reg in' (fst out)) `shouldBe` snd out ) in_n_out

  describe "pluralize" $ do
    it "handles the normal case" $ do
      pluralize "thing" `shouldBe` "things"

    it "pluralizes regex cases" $ do
      pluralize "ox" `shouldBe` "oxen"
      pluralize "vertex" `shouldBe` "vertices"

  describe "singularize" $ do
    it "singularizes regex cases" $ do
      singularize "mice" `shouldBe` "mouse"
      singularize "oxen" `shouldBe` "ox"
      singularize "branches" `shouldBe` "branch"


irregularCases :: Spec
irregularCases =
  it "can singularize irregulars" $
    singularize "people" `shouldBe` "person"
