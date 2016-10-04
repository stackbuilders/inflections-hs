-- |
-- Module      :  Text.Inflections.Countable
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Singularization and Pluralization of words

{-# LANGUAGE OverloadedStrings #-}

module Text.Inflections.Countable
  ( pluralize
  , pluralizeWith
  , singularize
  , singularizeWith
  , makeMatchMapping
  , makeIrregularMapping
  , makeUncountableMapping
  , Inflection(..)
  )
where

import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Inflections.Data
import Text.Regex.PCRE.Light
import Data.Monoid ((<>))

type RegexPattern = T.Text
type RegexReplace = T.Text
type Singular = T.Text
type Plural = T.Text

data Inflection
  = Simple (Singular, Plural)
  | Match (Maybe Regex, RegexReplace)

-- | Pluralize a word given a default mapping.
pluralize :: T.Text -> T.Text
pluralize = pluralizeWith mapping
  where
    mapping = defaultIrregulars ++ defaultUncountables ++ defaultPlurals

-- | Singularize a word given a default mapping.
singularize :: T.Text -> T.Text
singularize = singularizeWith mapping
  where
    mapping = defaultIrregulars ++ defaultUncountables ++ defaultSingulars

-- | Pluralize a word given a custom mapping.
-- Build the @['Inflection']@ with a combination of
-- `makeUncountableMapping` `makeIrregularMapping` `makeMatchMapping`.
pluralizeWith :: [Inflection] -> T.Text -> T.Text
pluralizeWith mapping t = fromMaybe t $ listToMaybe matches
  where
    matches = catMaybes $ fmap (pluralLookup t) (reverse mapping)

-- | Singularize a word given a custom mapping.
-- Build the @['Inflection']@ with a combination of
-- `makeUncountableMapping` `makeIrregularMapping` `makeMatchMapping`.
singularizeWith :: [Inflection] -> T.Text -> T.Text
singularizeWith mapping t = fromMaybe t $ listToMaybe matches
  where
    matches = catMaybes $ fmap (singularLookup t) (reverse mapping)

-- | Makes a simple list of mappings from singular to plural, e.g [("person", "people")]
-- the output of @['Inflection']@ should be consumed by `singularizeWith` or `pluralizeWith`
makeMatchMapping :: [(RegexPattern, RegexReplace)] -> [Inflection]
makeMatchMapping = fmap (\(pattern, rep) -> Match (regexPattern pattern, rep))

-- | Makes a simple list of mappings from singular to plural, e.g [("person", "people")]
-- the output of @['Inflection']@ should be consumed by `singularizeWith` or `pluralizeWith`
makeIrregularMapping :: [(Singular, Plural)] -> [Inflection]
makeIrregularMapping = fmap Simple

-- | Makes a simple list of uncountables which don't have
-- singular plural versions, e.g ["fish", "money"]
-- the output of @['Inflection']@ should be consumed by `singularizeWith` or `pluralizeWith`.
makeUncountableMapping :: [T.Text] -> [Inflection]
makeUncountableMapping = fmap (\a -> (Simple (a,a)))


defaultPlurals :: [Inflection]
defaultPlurals = makeMatchMapping defaultPlurals'

defaultSingulars :: [Inflection]
defaultSingulars = makeMatchMapping defaultSingulars'

defaultIrregulars :: [Inflection]
defaultIrregulars = makeIrregularMapping defaultIrregulars'

defaultUncountables :: [Inflection]
defaultUncountables = makeUncountableMapping defaultUncountables'

pluralLookup :: T.Text -> Inflection -> Maybe T.Text
pluralLookup t (Match (r1,r2)) = runSub (r1,r2) t
pluralLookup t (Simple (a,b)) = if t == a then (Just b) else Nothing

singularLookup :: T.Text -> Inflection -> Maybe T.Text
singularLookup t (Match (r1,r2)) = runSub (r1,r2) t
singularLookup t (Simple (a,b)) = if t == b then (Just a) else Nothing

runSub :: (Maybe Regex, RegexReplace) -> T.Text -> Maybe T.Text
runSub (Nothing, _) _ = Nothing
runSub (Just reg, rep) t = matchWithReplace (reg, rep) t

matchWithReplace :: (Regex, RegexReplace) -> T.Text -> Maybe T.Text
matchWithReplace (reg, rep) t = do
  matched : grouping <- regexMatch t reg
  return $ (t `without` matched) <> groupReplace grouping rep
  where
    without t1 t2 = if t2 == "" then t1 else T.replace t2 "" t1

groupReplace :: [T.Text] -> T.Text -> T.Text
groupReplace g rep =
  case g of
    [] -> rep
    (g':_) -> T.concat $ g' : additions
    where
      rep' = tailMaybe $ T.split ((==) '\1') rep
      additions = fromMaybe [] rep'

regexMatch :: T.Text -> Regex -> Maybe [T.Text]
regexMatch t r = do
  bs <- match r (encodeUtf8 t) []
  return $ fmap decodeUtf8 bs

regexPattern :: T.Text -> Maybe Regex
regexPattern pat = toMaybe $ compileM (encodeUtf8 pat) [caseless]
  where toMaybe = either (const Nothing) Just

tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe (_:xs) = Just xs
