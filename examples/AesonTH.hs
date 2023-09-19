{-# LANGUAGE TemplateHaskell #-}

module AesonTH where

import Data.Aeson (encode)
import Data.Aeson.TH
import Text.Megaparsec (errorBundlePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Text.Inflections as TI

{-
   In this example, I'm going to show how Text.Inflections can help us to format
   our JSON instances using Aeson.TH.
-}


-- | A common record
data MySnakedRecord =
  MySnakedRecord
    { attibuteNumberOne :: Int
    , attibuteNumberTwo :: String
    , otherAttributeOne :: Int
    , otherAttributeTwo :: String
    } deriving (Show, Eq)

-- | Use Aeson.TH to generate the instaces using Template Haskell.
-- The 'fieldLabelModifier' from 'defaultOptions' uses `TI.toUnderscore`
-- to format the attribute in a snake case.
$(deriveJSON
  (defaultOptions
    { fieldLabelModifier =
      let toSnake attribute =
            T.unpack $ fromRight' $ TI.toUnderscore (T.pack attribute)
          fromRight' (Right a) = a
          fromRight' (Left err) = error $ errorBundlePretty err
       in toSnake
    }
  )
  ''MySnakedRecord)

-- | This record example will be shown in snake case.
example :: MySnakedRecord
example =
  MySnakedRecord
    { attibuteNumberOne = 4
    , attibuteNumberTwo = "Hello"
    , otherAttributeOne = 2
    , otherAttributeTwo = "World!"
    }

{-
   >>> :set -package aeson
   >>> :set -package megaparsec
   >>> :set -package inflections
   >>> :set -package bytestring
   >>> :load AesonTH.hs
   >>> main
   "{ "attibute_number_one":4
    , "attibute_number_two":"Hello"
    , "other_attribute_one":2
    , "other_attribute_two":"World!"
    }"
-}

main :: IO ()
main = BL8.putStrLn (encode example)
