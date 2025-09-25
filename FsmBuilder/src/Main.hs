{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import FsmBuilder qualified as FB
import GraphBuilder qualified as GB
import InputBuilder qualified as IB
import OutputBuilder qualified as OB

-- Extract controllers from YAML file
extractControllers :: FilePath -> IO [IB.RawControllerInputState]
extractControllers configPath = do
  decodedConfig <- Yaml.decodeFileEither configPath
  case decodedConfig of
    Left configErr -> do
      putStrLn $ "Failed to parse YAML: " ++ show configErr
      return []
    Right configVal -> case configVal of
      Object configRoot -> case KM.lookup "controllers" configRoot of
        Just (Array controllersArr) -> do
          let controllers = IB.generateRawControllers controllersArr
          return controllers
        _ -> do
          putStrLn "No 'controllers' list in configuration"
          return []
      _ -> do
        putStrLn "Configuration root is not a mapping"
        return []

-- Extract controller outputs from YAML file
extractControllerOutputs :: FilePath -> IO [OB.RawControllerOutputState]
extractControllerOutputs configPath = do
  decodedConfig <- Yaml.decodeFileEither configPath
  case decodedConfig of
    Left configErr -> do
      putStrLn $ "Failed to parse YAML: " ++ show configErr
      return []
    Right configVal -> case configVal of
      Object configRoot -> case KM.lookup "controllers" configRoot of
        Just (Array controllersArr) -> do
          let controllerOutputs = OB.generateRawControllerOutputs controllersArr
          return controllerOutputs
        _ -> do
          putStrLn "No 'controllers' list in configuration"
          return []
      _ -> do
        putStrLn "Configuration root is not a mapping"
        return []

main :: IO ()
main = do
  -- inputs
  controllers <- extractControllers "./config/configuration.yml"
  print controllers -- Print the parsed controllers for debugging
  IB.main controllers

  -- outputs
  controllerOutputs <- extractControllerOutputs "./config/configuration.yml"
  print controllerOutputs -- Print the parsed controller outputs for debugging
  OB.main controllerOutputs

  -- Extract controller names for FSM builder
  let controllerNames = map IB.rawControllerName controllers ++ ["OutputState"]

  -- fsm states
  content <- readFile "./config/fsm.xml"
  let states = FB.parseXml content
  FB.main controllerNames controllers controllerOutputs states

  -- generate mermaid graph
  GB.main states

  putStrLn "All files generated successfully."