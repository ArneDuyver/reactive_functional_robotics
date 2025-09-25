{-# Limport Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.List (intercalate)
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import RfrUtilities (capitalize)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Mustache qualified as MustachetQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module OutputBuilder where

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.List (intercalate)
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import RfrUtilities (capitalize)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath (takeExtension, (</>))
import Text.Mustache qualified as Mustache

-- Intermediate representation for parsing controller outputs
data RawControllerOutputState = RawControllerOutputState
  { rawOutputControllerName :: String,
    rawOutputs :: [RawControllerOutput]
  }
  deriving (Show, Eq)

-- Intermediate representation for parsing controller outputs
data RawControllerOutput = RawControllerOutput
  { rawOutputName :: String,
    rawOutputType :: OutputType,
    rawOutputDefault :: String
  }
  deriving (Show, Eq)

-- Represents possible output types from the configuration
data OutputType
  = OutputInteger
  | OutputDouble
  | OutputString
  | OutputBool
  | OutputArray OutputType -- For nested arrays
  deriving (Show, Eq)

parseOutputTypeFromYaml :: String -> Maybe String -> OutputType
parseOutputTypeFromYaml "Integer" _ = OutputInteger
parseOutputTypeFromYaml "Int" _ = OutputInteger -- Handle both Integer and Int
parseOutputTypeFromYaml "Double" _ = OutputDouble
parseOutputTypeFromYaml "String" _ = OutputString
parseOutputTypeFromYaml "Boolean" _ = OutputBool
parseOutputTypeFromYaml "Array" (Just subType) = OutputArray (parseOutputTypeFromYaml subType Nothing)
parseOutputTypeFromYaml _ _ = OutputString

getOutputArrayType :: KM.KeyMap Aeson.Value -> Maybe String
getOutputArrayType obj = case KM.lookup "arrayType" obj of
  Just (String s) -> Just (T.unpack s)
  _ -> Nothing

parseRawOutput :: Aeson.Value -> RawControllerOutput
parseRawOutput (Object m) =
  case KM.toList m of
    ((_, Object outputObj) : _) ->
      RawControllerOutput
        { rawOutputName = getText "name",
          rawOutputType = parseOutputTypeFromYaml (getText "type") (getOutputArrayType outputObj),
          rawOutputDefault = getDefault "default"
        }
      where
        getText key = case KM.lookup key outputObj of
          Just (String s) -> T.unpack s
          _ -> ""
        getDefault key = case KM.lookup key outputObj of
          Just (String s) -> "\"" ++ T.unpack s ++ "\"" -- String values need quotes
          Just (Number n) ->
            let fieldType = getText "type"
             in if fieldType == "Double"
                  then show (Scientific.toRealFloat n :: Double) -- Always format as Double for Double fields
                  else
                    if Scientific.isInteger n
                      then show (Scientific.coefficient n) -- For integers
                      else show (Scientific.toRealFloat n :: Double) -- For other numeric types
          Just (Bool b) -> if b then "True" else "False"
          _ -> error $ "No default value provided for output: " ++ getText "name"
    _ -> error "Invalid output structure in YAML"
parseRawOutput _ = error "Invalid output structure in YAML"

-- Parse controller Key Map values into RawControllerOutputState
parseControllerOutputs :: Aeson.Value -> RawControllerOutputState
parseControllerOutputs (Object controllerObj) =
  case KM.toList controllerObj of
    ((_, Object metaControllerObj) : _) ->
      RawControllerOutputState
        { rawOutputControllerName = case KM.lookup "name" metaControllerObj of
            Just (String s) -> T.unpack s
            _ -> "DefaultParseControllerName",
          rawOutputs = case KM.lookup "outputs" metaControllerObj of
            Just (Array outputs) -> map parseRawOutput (V.toList outputs)
            _ -> []
        }
    _ -> RawControllerOutputState "DefaultParseControllerName_KeyMapControllerListObject" []
parseControllerOutputs _ = RawControllerOutputState "DefaultParseControllerName_KeyMapControllerObject" []

-- Generate RawControllerOutputState from YAML controller entries
generateRawControllerOutputs :: V.Vector Aeson.Value -> [RawControllerOutputState]
generateRawControllerOutputs controllersArr =
  let allControllers = map parseControllerOutputs (V.toList controllersArr)
      controllersWithOutputs = filter (not . null . rawOutputs) allControllers
   in controllersWithOutputs

-- Generate the OutputState.hs file with all controller output data types
generateOutputState :: [RawControllerOutputState] -> IO ()
generateOutputState controllers = do
  let controllerDataTypes = map generateControllerDataType controllers
      outputDataFields = map generateOutputDataField controllers
      controllerDefaults = map generateControllerDefault controllers

  -- load template and render
  eTemplate <- Mustache.automaticCompile ["./FsmBuilder/templates"] "outputstate.mustache"
  case eTemplate of
    Left err -> putStrLn $ "Template parse error: " ++ show err
    Right template -> do
      let context =
            Aeson.object
              [ "output_datatypes" Aeson..= intercalate "\n\n" controllerDataTypes,
                "outputs" Aeson..= intercalate "\n  , " outputDataFields,
                "controller_defaults" Aeson..= intercalate "\n\n" controllerDefaults
              ]
      let rendered = Mustache.substitute template context
      let outDir = "FsmRunner/src/Helpers/Controllers"
      let outFile = outDir </> "OutputState.hs"
      createDirectoryIfMissing True outDir
      -- replace HTML entity &quot; with real double-quote and write
      let fixed = T.replace "&quot;" "\"" rendered
      TIO.writeFile outFile fixed
      putStrLn $ "Wrote " ++ outFile
  where
    generateControllerDataType :: RawControllerOutputState -> String
    generateControllerDataType controller =
      let controllerName = rawOutputControllerName controller
          controllerNameCap = capitalize controllerName
          outputs = rawOutputs controller
          outputFields =
            map
              ( \output ->
                  rawOutputName output ++ " :: " ++ outputTypeToHaskellType (rawOutputType output)
              )
              outputs
          fieldsStr = intercalate "\n  , " outputFields
          dataTypeDecl = "data " ++ controllerNameCap ++ " = " ++ controllerNameCap ++ "\n  { " ++ fieldsStr ++ "\n  } deriving (Show, Eq, Generic)"
          toJsonInstance = "instance ToJSON " ++ controllerNameCap
       in dataTypeDecl ++ "\n\n" ++ toJsonInstance

    generateOutputDataField :: RawControllerOutputState -> String
    generateOutputDataField controller =
      let controllerName = rawOutputControllerName controller
          controllerNameCap = capitalize controllerName
       in controllerName ++ " :: " ++ controllerNameCap

    generateControllerDefault :: RawControllerOutputState -> String
    generateControllerDefault controller =
      let controllerName = rawOutputControllerName controller
          controllerNameCap = capitalize controllerName
          outputs = rawOutputs controller
          defaultFields =
            map
              ( \output ->
                  rawOutputName output ++ " = " ++ rawOutputDefault output
              )
              outputs
          fieldsStr = intercalate "\n  , " defaultFields
          defaultInstance =
            "default"
              ++ controllerNameCap
              ++ " :: "
              ++ controllerNameCap
              ++ "\n"
              ++ "default"
              ++ controllerNameCap
              ++ " = "
              ++ controllerNameCap
              ++ "\n  { "
              ++ fieldsStr
              ++ "\n  }"
       in defaultInstance

    outputTypeToHaskellType :: OutputType -> String
    outputTypeToHaskellType OutputInteger = "Integer"
    outputTypeToHaskellType OutputDouble = "Double"
    outputTypeToHaskellType OutputString = "String"
    outputTypeToHaskellType OutputBool = "Bool"
    outputTypeToHaskellType (OutputArray innerType) = "[" ++ outputTypeToHaskellType innerType ++ "]"

main :: [RawControllerOutputState] -> IO ()
main rawControllerOutputs = generateOutputState rawControllerOutputs