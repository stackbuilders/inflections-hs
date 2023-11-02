import qualified Data.Text as T
import System.Random (randomRIO)
import qualified Text.Inflections as TI

{-
    This example creates a random user name based on the user name and
    last name (E.g. Andres Lalvay -> andres-lalvay-83). This is using
    the inflections library (parameterize).
-}

-- Generate random user name with a random number
generateRandomUserName :: T.Text -> T.Text -> IO T.Text
generateRandomUserName firstName lastName = do
  randomNumberGenerated <- randomRIO (10, 99)
  let formattedFirstName = TI.parameterize firstName
      formattedLastName = TI.parameterize lastName
      randomStr = T.pack $ show (randomNumberGenerated :: Int)
      separator = T.pack "-"
      username = T.toLower $ T.intercalate separator [formattedFirstName, formattedLastName, randomStr]
  return username

-- Receives user's name and last name
main :: IO ()
main = do
  putStrLn "Please enter your first name: "
  firstName <- getLine

  putStrLn "Please enter your last name: "
  lastName <- getLine

  let usernameText = generateRandomUserName (T.pack firstName) (T.pack lastName)
  username <- usernameText

  putStrLn $ "Generated username: " ++ T.unpack username

{-
    >>> :set -package text
    >>> :set -package inflections
    >>> :set -package random
    >>> main
-}

{- 
    As part of Stack Builders Inc. Hacktoberfest 2023 event.
    Visit Stack Builders Inc at: www.stackbuilders.com
-}