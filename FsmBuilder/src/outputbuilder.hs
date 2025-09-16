{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module OutputBuilder where

import RfrUtilities (capitalize)
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Text.Mustache as Mustache
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Scientific as Scientific
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- Intermediate representation for parsing controller outputs
data RawControllerOutputState = RawControllerOutputState
  { rawOutputControllerName :: String
  , rawOutputs :: [RawControllerOutput]
  } deriving (Show, Eq)

-- Intermediate representation for parsing controller outputs
data RawControllerOutput = RawControllerOutput
  { rawOutputName :: String
  , rawOutputType :: OutputType
  } deriving (Show, Eq)

-- Represents possible output types from the configuration
data OutputType = 
    OutputInteger
  | OutputDouble
  | OutputString
  | OutputBool
  | OutputArray OutputType  -- For nested arrays
  deriving (Show, Eq)

parseOutputTypeFromYaml :: String -> Maybe String -> OutputType
parseOutputTypeFromYaml "Integer" _ = OutputInteger
parseOutputTypeFromYaml "Int" _ = OutputInteger  -- Handle both Integer and Int
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
    ((_, Object outputObj):_) ->
      RawControllerOutput
        { rawOutputName = getText "name"
        , rawOutputType = parseOutputTypeFromYaml (getText "type") (getOutputArrayType outputObj)
        }
      where
        getText key = case KM.lookup key outputObj of
          Just (String s) -> T.unpack s
          _ -> ""
    _ -> RawControllerOutput "" OutputString
parseRawOutput _ = RawControllerOutput "" OutputString

-- Parse controller Key Map values into RawControllerOutputState
parseControllerOutputs :: Aeson.Value -> RawControllerOutputState
parseControllerOutputs (Object controllerObj) =
  case KM.toList controllerObj of
    ((_, Object metaControllerObj):_) -> 
      RawControllerOutputState 
        { rawOutputControllerName = case KM.lookup "name" metaControllerObj of
            Just (String s) -> T.unpack s
            _ -> "DefaultParseControllerName"
        , rawOutputs = case KM.lookup "outputs" metaControllerObj of
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
      
  -- load template and render
  eTemplate <- Mustache.automaticCompile ["./FsmBuilder/templates"] "outputstate.mustache"
  case eTemplate of
    Left err -> putStrLn $ "Template parse error: " ++ show err
    Right template -> do
      let context = Aeson.object
            [ "output_datatypes" Aeson..= intercalate "\n\n" controllerDataTypes
            , "outputs" Aeson..= intercalate "\n  , " outputDataFields
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
          outputFields = map (\output -> 
            rawOutputName output ++ " :: " ++ outputTypeToHaskellType (rawOutputType output)
            ) outputs
          fieldsStr = intercalate "\n  , " outputFields
          dataTypeDecl = "data " ++ controllerNameCap ++ " = " ++ controllerNameCap ++ "\n  { " ++ fieldsStr ++ "\n  } deriving (Show, Eq, Generic)"
          toJsonInstance = "instance ToJSON " ++ controllerNameCap
      in dataTypeDecl ++ "\n\n" ++ toJsonInstance
    
    generateOutputDataField :: RawControllerOutputState -> String
    generateOutputDataField controller = 
      let controllerName = rawOutputControllerName controller
          controllerNameCap = capitalize controllerName
      in controllerName ++ " :: " ++ controllerNameCap
    
    outputTypeToHaskellType :: OutputType -> String
    outputTypeToHaskellType OutputInteger = "Integer"
    outputTypeToHaskellType OutputDouble = "Double"
    outputTypeToHaskellType OutputString = "String"
    outputTypeToHaskellType OutputBool = "Bool"
    outputTypeToHaskellType (OutputArray innerType) = "[" ++ outputTypeToHaskellType innerType ++ "]"

main :: [RawControllerOutputState] -> IO ()
main rawControllerOutputs = generateOutputState rawControllerOutputs