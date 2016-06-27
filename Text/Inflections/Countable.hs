module Text.Inflections.Countable where

import qualified Data.Bimap as Bimap
import Data.Maybe (fromMaybe)
import Data.Text
import Text.Inflections.Data

pluralize :: Text -> Text
pluralize = pluralizeWith simpleCountableMap

singularize :: Text -> Text
singularize = singularizeWith simpleCountableMap

pluralizeWith :: [(Text, Text)] -> Text -> Text
pluralizeWith mapping t =
  fromMaybe t (Bimap.lookup t (Bimap.fromList mapping))

singularizeWith :: [(Text, Text)] -> Text -> Text
singularizeWith mapping t =
  fromMaybe t (Bimap.lookupR t (Bimap.fromList mapping))
