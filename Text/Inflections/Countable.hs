module Text.Inflections.Countable where

import qualified Data.Bimap as Bimap
import Data.Maybe (fromMaybe)
import Data.Text
import Text.Inflections.Data

pluralize :: Text -> Text
pluralize t = fromMaybe t (Bimap.lookup t simpleCountableMap)

singularize :: Text -> Text
singularize t = fromMaybe t (Bimap.lookupR t simpleCountableMap)
