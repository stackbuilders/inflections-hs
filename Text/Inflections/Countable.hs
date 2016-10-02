{-# LANGUAGE OverloadedStrings #-}
module Text.Inflections.Countable where

import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Inflections.Data
import Text.Regex.PCRE.Light
import Data.Monoid ((<>))
import Data.ByteString.Char8

type RegexPattern = T.Text
type RegexReplace = T.Text
type Singular = T.Text
type Plural = T.Text

data Inflection
  = Simple (Singular, Plural)
  | Match (RegexPattern, RegexReplace)

pluralize :: T.Text -> T.Text
pluralize = pluralizeWith mapping
  where
    mapping = defaultIrregulars ++ defaultUncountables ++ defaultPlurals

singularize :: T.Text -> T.Text
singularize = singularizeWith mapping
  where
    mapping = defaultIrregulars ++ defaultUncountables ++ defaultSingulars

pluralizeWith :: [Inflection] -> T.Text -> T.Text
pluralizeWith mapping t = fromMaybe t $ headMaybe matches
  where
    matches = catMaybes $ fmap (pluralLookup t) (Prelude.reverse mapping)

singularizeWith :: [Inflection] -> T.Text -> T.Text
singularizeWith mapping t = fromMaybe t $ headMaybe matches
  where
    matches = catMaybes $ fmap (singularLookup t) (Prelude.reverse mapping)

pluralLookup :: T.Text -> Inflection -> Maybe T.Text
pluralLookup t (Match (r1,r2)) = runSub (r1,r2) t
pluralLookup t (Simple (a,b)) = if t == a then (Just b) else Nothing

singularLookup :: T.Text -> Inflection -> Maybe T.Text
singularLookup t (Match (r1,r2)) = runSub (r1,r2) t
singularLookup t (Simple (a,b)) = if t == b then (Just a) else Nothing

runSub :: (RegexPattern, RegexReplace) -> T.Text -> Maybe T.Text
runSub (pat, rep) t = case (regexPattern pat, rep) of
  (Nothing, _) -> Nothing
  (Just reg, rep) -> matchWithReplace (reg, rep) t

matchWithReplace :: (Regex, RegexReplace) -> T.Text -> Maybe T.Text
matchWithReplace (reg, rep) t = case regexMatch t reg of
  Nothing -> Nothing
  Just (matched : grouping) ->
    pure $ (t `without` matched) <> groupReplace grouping
  where
    groupReplace g = fromMaybe "" $ headMaybe g <> addition
    addition = headMaybe $ fromMaybe [] $ tailMaybe $ T.split ((==) '\1') rep

without :: T.Text -> T.Text -> T.Text
without t1 t2 = if t2 == "" then t1 else T.replace t2 "" t1

regexMatch :: T.Text -> Regex -> Maybe [T.Text]
regexMatch t r = case match r t' [] of
  Just bs -> Just $ fmap decodeUtf8 bs
  Nothing -> Nothing
  where
    t' = encodeUtf8 t

regexPattern :: T.Text -> Maybe Regex
regexPattern pat = eitherToMaybe $ compileM (encodeUtf8 pat) [caseless]

eitherToMaybe e = case e of
  Left _ -> Nothing
  Right r -> Just r

headMaybe [] = Nothing
headMaybe (x:_) = Just x

tailMaybe [] = Nothing
tailMaybe (_:xs) = Just xs

makeMatchMapping :: [(T.Text, T.Text)] -> [Inflection]
makeMatchMapping = fmap Match
  --TODO compile the regex

makeIrregularMapping :: [(T.Text, T.Text)] -> [Inflection]
makeIrregularMapping = fmap Simple

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
