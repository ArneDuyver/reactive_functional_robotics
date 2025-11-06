module RfrUtilities where

import Data.Char (toUpper)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Small utility: capitalize first letter
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- Parse a .env file and return a list of key-value pairs
parseEnvFile :: FilePath -> IO [(String, String)]
parseEnvFile filePath = do
  content <- TIO.readFile filePath
  let linesOfFile = lines (T.unpack content)
  let envLines = filter (not . isCommentOrEmpty) linesOfFile
  return $ map parseEnvLine envLines
  where
    isCommentOrEmpty line = null (dropWhile (== ' ') line) || head (dropWhile (== ' ') line) == '#'
    parseEnvLine line = 
      case splitOn "=" line of
        (key:value:_) -> (trim key, trim (concat $ splitOn "=" (unwords [value])))
        [key] -> (trim key, "")
        [] -> ("", "")
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- Get a specific value from the environment file
getEnvValue :: FilePath -> String -> IO (Maybe String)
getEnvValue filePath key = do
  envVars <- parseEnvFile filePath
  return $ lookup key envVars

-- Get MQTT broker configuration from .env file
getMqttConfig :: FilePath -> IO (Maybe (String, String, String, String))
getMqttConfig filePath = do
  envVars <- parseEnvFile filePath
  let host = lookup "MQTT_BROKER_HOST" envVars
  let port = lookup "MQTT_BROKER_PORT" envVars
  let fromTopic = lookup "FROM_ENV_COLLECTION_TOPIC" envVars
  let toTopic = lookup "TO_ENV_COLLECTION_TOPIC" envVars
  case (host, port, fromTopic, toTopic) of
    (Just h, Just p, Just ft, Just tt) -> return $ Just (h, p, ft, tt)
    _ -> return Nothing

