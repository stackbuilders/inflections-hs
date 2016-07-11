{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.ParametrizeSpec (spec) where

import Data.Char (toLower)
import Data.List (group)
import Test.Hspec
import Test.QuickCheck
import Text.Inflections
import qualified Data.Text as T

spec :: Spec
spec =
  describe "parameterize" $ do
    it "returns only valid characters" $ property $ \sf ->
      T.all (`elem` (alphaNumerics ++ "-_")) $ parameterize (T.pack sf)
    it "never returns a string beginning with a separator" $ property $ \s ->
      let parameterized = parameterize (T.pack s)
      in (not . T.null) parameterized ==> T.head parameterized /= '-'
    it "never returns a string ending with a separator" $ property $ \s ->
      let parameterized = parameterize (T.pack s) in
      (not . T.null) parameterized ==> T.last parameterized /= '-'
    it "returns every alphanumeric character from the input" $ property $ \s ->
      let parameterized = parameterize (T.pack s)
      in all (\c -> c `notElem` alphaNumerics ||
           c `elem` (alphaNumerics ++ "-") &&
           c `elem` T.unpack parameterized) $ map toLower s
    it "never returns a string with a sequence of dashes" $ property $ \s ->
      let parameterized = parameterize (T.pack s)
      in longestSequenceOf '-' (T.unpack parameterized) <= 1

longestSequenceOf :: Char -> String -> Int
longestSequenceOf _ [] = 0
longestSequenceOf c s =
    if null subseqLengths then 0 else maximum subseqLengths
  where subseqLengths = (map length . filter (\str -> head str == c) . group) s

alphaNumerics :: String
alphaNumerics = ['a'..'z'] ++ ['0'..'9']
