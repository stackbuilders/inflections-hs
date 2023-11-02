import qualified Data.Text as T
import System.Random (randomRIO)
import qualified Text.Inflections as TI

{-
  This example is explained in detail in the README.md file inside
  the random-username project folder.
-}

makeUserNameWith :: [T.Text] -> T.Text
makeUserNameWith = T.intercalate (T.pack "-")

generateRandomUserName :: T.Text -> T.Text -> IO T.Text
generateRandomUserName firstName lastName = do
  randomNumber <- T.toLower . T.pack . show <$> randomRIO (10 :: Int, 99)
  let formattedFirstName = TI.parameterize firstName
      formattedLastName = TI.parameterize lastName
      nameParts = [formattedFirstName, formattedLastName, randomNumber]
  return $ makeUserNameWith nameParts

main :: IO ()
main = do
  putStrLn "Please enter your first name: "
  firstName <- T.pack <$> getLine

  putStrLn "Please enter your last name: "
  lastName <- T.pack <$> getLine

  let usernameText = generateRandomUserName firstName lastName
  username <- T.unpack <$> usernameText

  putStrLn $ "Generated username: " ++ username
