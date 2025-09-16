{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module InputBuilder where

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

-- Intermediate representation for parsing
data RawControllerInputState = RawControllerInputState
  { rawControllerName :: String
  , rawCritical :: Bool
  , rawInputs :: [RawControllerInputs]
  } deriving (Show, Eq)

-- Intermediate representation for parsing controller inputs
data RawControllerInputs = RawControllerInputs
  { rawInputName :: String
  , rawInputType :: InputType
  , rawDefault :: Maybe DefaultValue
  } deriving (Show, Eq)

-- Represents possible input types from the configuration
data InputType = 
    InputInteger
  | InputDouble
  | InputString
  | InputBool
  | InputArray InputType  -- For nested arrays
  deriving (Show, Eq)

-- Represents possible default values matching the input types
data DefaultValue = 
    DefaultInteger Integer
  | DefaultDouble Double
  | DefaultString String
  | DefaultBool Bool
  | DefaultArray [DefaultValue]
  | DefaultNull
  | DefaultInvalid  -- for invalid/unsupported values
  deriving (Show, Eq)

parseInputTypeFromYaml :: String -> Maybe String -> InputType
parseInputTypeFromYaml "Integer" _ = InputInteger
parseInputTypeFromYaml "Double" _ = InputDouble
parseInputTypeFromYaml "String" _ = InputString
parseInputTypeFromYaml "Boolean" _ = InputBool
parseInputTypeFromYaml "Array" (Just subType) = InputArray (parseInputTypeFromYaml subType Nothing)
parseInputTypeFromYaml _ _ = InputString

-- Convert Aeson Value to our DefaultValue type
parseDefaultValue :: Value -> DefaultValue
parseDefaultValue (String s) = DefaultString (T.unpack s)
parseDefaultValue (Number n) = 
    case Scientific.floatingOrInteger n of
        Right i -> DefaultInteger i
        Left d -> DefaultDouble d
parseDefaultValue (Bool b) = DefaultBool b
parseDefaultValue (Array arr) = DefaultArray (map parseDefaultValue (V.toList arr))
parseDefaultValue Null = DefaultNull
parseDefaultValue _ = DefaultInvalid

getArrayType :: KM.KeyMap Aeson.Value -> Maybe String
getArrayType obj = case KM.lookup "arrayType" obj of
  Just (String s) -> Just (T.unpack s)
  _ -> Nothing

parseRawInput :: Aeson.Value -> RawControllerInputs
parseRawInput (Object m) = 
  case KM.toList m of
    ((_, Object inputObj):_) ->
      RawControllerInputs
        { rawInputName = getText "name"
        , rawInputType = parseInputTypeFromYaml (getText "type") (getArrayType inputObj)
        , rawDefault = parseDefaultValue <$> KM.lookup "default" inputObj
        }
      where
        getText key = case KM.lookup key inputObj of
          Just (String s) -> T.unpack s
          _ -> ""
    _ -> RawControllerInputs "" InputString Nothing
parseRawInput _ = RawControllerInputs "" InputString Nothing

-- Parse controller Key Map values into RawControllerInputState s
parseController :: Aeson.Value -> RawControllerInputState
parseController (Object controllerObj) =
  case KM.toList controllerObj of
    ((_, Object metaControllerObj):_) -> 
      RawControllerInputState 
        { rawControllerName = case KM.lookup "name" metaControllerObj of
            Just (String s) -> T.unpack s
            _ -> "DefaultParseControllerName"
        , rawCritical = case KM.lookup "critical" metaControllerObj of
            Just (Bool b) -> b
            _ -> False
        , rawInputs = case KM.lookup "inputs" metaControllerObj of
            Just (Array inputs) -> map parseRawInput (V.toList inputs)
            _ -> []
        }
    _ -> RawControllerInputState "DefaultParseControllerName_KeyMapControllerListObject" False []
parseController _ = RawControllerInputState "DefaultParseControllerName_KeyMapControllerObject" False []

-- Generate RawControllerInputState from YAML controller entries
generateRawControllers :: V.Vector Aeson.Value -> [RawControllerInputState]
generateRawControllers controllersArr = map parseController (V.toList controllersArr)


-- Build controller context and render template
generateController :: RawControllerInputState -> IO ()
generateController controller = do
  let controllername = rawControllerName controller
  let controllernameCapitalized = capitalize controllername

  -- Convert RawControllerInputs to the format needed for template
  let convertInput input = 
        ( rawInputName input
        , inputTypeToHaskellType (rawInputType input)
        , maybe "\"\"" defaultValueToString (rawDefault input)
        )
      
  let parsedInputs = map convertInput (rawInputs controller)
  let defaultFor = "\"\"" -- empty string literal as safe default
  let controllerInputs = intercalate "\n  , " $ map (\(n,t,_) -> n ++ " :: " ++ t) parsedInputs
  let controllerInputDefaults = intercalate "\n  , " $ map (\(n,_,md) -> n ++ " = " ++ md) parsedInputs

  -- load template and render
  eTemplate <- Mustache.automaticCompile ["./FsmBuilder/templates"] "controller.mustache"
  case eTemplate of
    Left err -> putStrLn $ "Template parse error: " ++ show err
    Right template -> do
      let context = Aeson.object
            [ "controllername" Aeson..= controllername
            , "controllernameCapitalized" Aeson..= controllernameCapitalized
            , "controllerInputs" Aeson..= controllerInputs
            , "controllerInputDefaults" Aeson..= controllerInputDefaults
            ]
      let rendered = Mustache.substitute template context
      let outDir = "FsmRunner/src/Helpers/Controllers"
      let outFile = outDir </> controllernameCapitalized ++ ".hs"
      createDirectoryIfMissing True outDir
      -- replace HTML entity &quot; with real double-quote and write
      let fixed = T.replace "&quot;" "\"" rendered
      TIO.writeFile outFile fixed
      putStrLn $ "Wrote " ++ outFile

  where
    inputTypeToHaskellType :: InputType -> String
    inputTypeToHaskellType InputInteger = "Integer"
    inputTypeToHaskellType InputDouble = "Double"
    inputTypeToHaskellType InputString = "String"
    inputTypeToHaskellType InputBool = "Bool"
    inputTypeToHaskellType (InputArray innerType) = "[" ++ inputTypeToHaskellType innerType ++ "]"

    defaultValueToString :: DefaultValue -> String
    defaultValueToString (DefaultInteger i) = show i
    defaultValueToString (DefaultDouble d) = show d
    defaultValueToString (DefaultString s) = show s
    defaultValueToString (DefaultBool b) = show b
    defaultValueToString (DefaultArray values) = "[" ++ intercalate "," (map defaultValueToString values) ++ "]"
    defaultValueToString DefaultNull = "\"\""
    defaultValueToString DefaultInvalid = "\"\""


main :: [RawControllerInputState] -> IO ()
main rawControllers = mapM_ generateController rawControllers