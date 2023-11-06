{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TO
import System.Random (randomRIO)
import qualified Text.Inflections as TI

{-
  This example is elaborated upon in the README.md file located inside
  the random-username project folder.
-}

makeUserNameWith :: [T.Text] -> T.Text
makeUserNameWith = T.intercalate "-"

generateRandomUserName :: T.Text -> T.Text -> IO T.Text
generateRandomUserName firstName lastName = do
  randomNumber <- (T.toLower . T.pack . show) <$> randomRIO @Int (10, 99)
  let formattedFirstName = TI.parameterize firstName
      formattedLastName = TI.parameterize lastName
      nameParts = [formattedFirstName, formattedLastName, randomNumber]
  return $ makeUserNameWith nameParts

main :: IO ()
main = do
  putStrLn "Please enter your first name: "
  firstName <- TO.getLine

  putStrLn "Please enter your last name: "
  lastName <- TO.getLine

  username <- generateRandomUserName firstName lastName

  TO.putStrLn $ "Generated username: " <> username
