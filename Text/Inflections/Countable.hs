module Text.Inflections.Countable where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Text
import Text.Inflections.Data
import Text.Regex.TDFA.ByteString as Regex

type RegexPattern = Text
type RegexReplace = Text
type Singular = Text
type Plural = Text

data Inflection
  = Simple (Singular, Plural)
  | Match (RegexPattern, RegexReplace)

pluralize :: Text -> Text
pluralize = pluralizeWith mapping
  where
    mapping = defaultIrregulars ++ defaultUncountables ++ defaultPlurals

singularize :: Text -> Text
singularize = singularizeWith mapping
  where
    mapping = defaultIrregulars ++ defaultUncountables ++ defaultSingulars

pluralizeWith :: [Inflection] -> Text -> Text
pluralizeWith mapping t = fromMaybe t $ headMaybe matches
  where
    matches = catMaybes $ fmap (pluralLookup t) mapping 

singularizeWith :: [Inflection] -> Text -> Text
singularizeWith mapping t = fromMaybe t $ headMaybe matches
  where
    matches = catMaybes $ fmap (singularLookup t) mapping

pluralLookup :: Text -> Inflection -> Maybe Text
pluralLookup t (Match (r1,r2)) = Nothing
pluralLookup t (Simple (a,b)) = if t == a then (Just b) else Nothing

singularLookup :: Text -> Inflection -> Maybe Text
singularLookup t (Match (r1,r2)) = Nothing
singularLookup t (Simple (a,b)) = if t == b then (Just a) else Nothing

headMaybe [] = Nothing
headMaybe (x:_) = Just x

makeMatchMapping :: [(Text, Text)] -> [Inflection]
makeMatchMapping = fmap Match

makeIrregularMapping :: [(Text, Text)] -> [Inflection]
makeIrregularMapping = fmap Simple

makeUncountableMapping :: [Text] -> [Inflection]
makeUncountableMapping = fmap (\a -> (Simple (a,a)))

defaultPlurals :: [Inflection]
defaultPlurals = makeMatchMapping defaultPlurals'

defaultSingulars :: [Inflection]
defaultSingulars = makeMatchMapping defaultSingulars'

defaultIrregulars :: [Inflection]
defaultIrregulars = makeIrregularMapping defaultIrregulars'

defaultUncountables :: [Inflection]
defaultUncountables = makeUncountableMapping defaultUncountables'

-- regexPattern :: Text -> Either String Regex.Regex
-- regexPattern bs = Regex.compile comp ex bs
--   where
--     comp = defaultCompOpt { caseSensitive = False }
--     ex = ExecOption { captureGroups = True }

-- regexMatch :: Text -> Bool
-- regexMatch bs =
--     case regexMatch' txt of
--       Left _ -> False
--       Right mmatch -> case mmatch of
--                         Just a -> True
--                         Nothing -> False

-- regexMatch' p =
--       Regex.execute (regexPattern p)
--     . ByteString.Lazy.toStrict
