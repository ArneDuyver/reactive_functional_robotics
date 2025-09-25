{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module GraphBuilder where

import Data.Aeson qualified as Aeson
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import FsmBuilder (State (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Mustache qualified as Mustache

-- Generate JavaScript array format for state names (always include Error state)
generateStateNames :: [State] -> String
generateStateNames states =
  let stateNames = map name states
      allStateNames = if "Error" `elem` stateNames then stateNames else stateNames ++ ["Error"]
      quotedNames = map (\s -> "\"" ++ s ++ "\"") allStateNames
   in "[" ++ intercalate ", " quotedNames ++ "]"

-- Generate JavaScript array format for state associations (transitions)
generateStateAssociations :: [State] -> String
generateStateAssociations states =
  let associations = concatMap (\st -> map (\tr -> "[\"" ++ name st ++ "\", \"" ++ tr ++ "\"]") (transitions st)) states
   in "[" ++ intercalate ", " associations ++ "]"

-- Generate the start state (first state in the list)
generateStartState :: [State] -> String
generateStartState [] = "null"
generateStartState (s : _) = "\"" ++ name s ++ "\""

-- Parse a simple .env file format
parseEnvFile :: String -> [(String, String)]
parseEnvFile content =
  let linesOfFile = lines content
      nonEmptyLines = filter (not . null) linesOfFile
      nonCommentLines = filter (not . startsWith "#") nonEmptyLines
      keyValuePairs = mapMaybe parseKeyValue nonCommentLines
   in keyValuePairs
  where
    startsWith prefix str = take (length prefix) str == prefix
    parseKeyValue line =
      case break (== '=') line of
        (key, '=' : value) -> Just (trim key, trim value)
        _ -> Nothing
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    mapMaybe f xs = [y | x <- xs, Just y <- [f x]]

-- Extract MQTT configuration from .env file
getMqttConfig :: FilePath -> IO (String, String)
getMqttConfig envPath = do
  content <- readFile envPath
  let envVars = parseEnvFile content
      brokerHost = fromMaybe "ws://localhost" (lookup "MQTT_BROKER_HOST_JS" envVars)
      brokerPort = fromMaybe "9001" (lookup "MQTT_BROKER_PORT_JS" envVars)
      topicHighlight = fromMaybe "fsm/toenv" (lookup "TO_ENV_COLLECTION_TOPIC" envVars)
      brokerUrl = brokerHost ++ ":" ++ brokerPort
  return (brokerUrl, topicHighlight)

-- Generate the mermaid-mqtt.js file using the template
generateMermaidMqttJs :: [State] -> IO ()
generateMermaidMqttJs states = do
  let stateNames = generateStateNames states
      stateAssociations = generateStateAssociations states
      startState = generateStartState states

  -- Get MQTT configuration from .env file
  (brokerUrl, topicHighlight) <- getMqttConfig "./config/.env"

  -- Load template and render
  eTemplate <- Mustache.automaticCompile ["./FsmBuilder/templates"] "mermaid-mqtt.mustache"
  case eTemplate of
    Left err -> putStrLn $ "Template parse error: " ++ show err
    Right template -> do
      let context =
            Aeson.object
              [ "state_names" Aeson..= stateNames,
                "state_associations" Aeson..= stateAssociations,
                "start_state" Aeson..= startState,
                "broker_url" Aeson..= brokerUrl,
                "topic_highlight" Aeson..= topicHighlight
              ]
      let rendered = Mustache.substitute template context
      let outDir = "FsmFrontend/static"
      let outFile = outDir </> "mermaid-mqtt.js"
      createDirectoryIfMissing True outDir
      -- Replace HTML entity &quot; with real double-quote and write
      let fixed = T.replace "&quot;" "\"" rendered
      TIO.writeFile outFile fixed
      putStrLn $ "Wrote " ++ outFile ++ " with broker: " ++ brokerUrl ++ " and topic: " ++ topicHighlight

main :: [State] -> IO ()
main states = generateMermaidMqttJs states