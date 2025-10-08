{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module FsmBuilder (State (..), parseXml, main) where

import Control.Monad (filterM)
import Data.Aeson qualified as Aeson
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text qualified as T
import InputBuilder qualified as IB
import MixedBuilder qualified as MB
import OutputBuilder qualified as OB
import RfrUtilities (capitalize)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile, doesFileExist, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath (takeExtension, (</>))
import Text.Mustache qualified as Mustache
import Text.XML.Light
import Text.XML.Light.Output (showElement)

-- TODO: do not make it overwrite existing files!!!

-- Intermediate representation for parsing
data RawState = RawState
  { rawId :: String,
    rawName :: String
  }
  deriving (Show, Eq)

-- Intermediate representation for parsing
data State = State
  { name :: String,
    transitions :: [String]
  }
  deriving (Show, Eq)

-- Intermediate representation for parsing
data RawTransition = RawTransition
  { rawSourceId :: String,
    rawTargetId :: String
  }
  deriving (Show, Eq)

-- Recursively get all elements in the XML tree (elChildren: returns a list of child Elements for a given XML Element. concatMap: apply universe to every element and concatenate into one list)
universe :: Element -> [Element]
universe e = e : concatMap universe (elChildren e)

-- Get the "target" attribute from the first mxCell element that has BOTH a "value" and a "target" attribute
getStartState :: [Element] -> String
getStartState els = case filter (\el -> isJust (lookupAttr (unqual "value") (elAttribs el)) && isJust (lookupAttr (unqual "target") (elAttribs el))) els of
  (el : _) -> fromMaybe "" (lookupAttr (unqual "target") (elAttribs el))
  [] -> ""

-- Get the list of RawState from mxCell elements that have a "value" attribute and no "target" attribute
getStates :: [Element] -> [RawState]
getStates = map toRawState . filter (\el -> isJust (lookupAttr (unqual "value") (elAttribs el)) && isNothing (lookupAttr (unqual "target") (elAttribs el)))
  where
    toRawState el = RawState {rawId = fromMaybe "" (lookupAttr (unqual "id") (elAttribs el)), rawName = fromMaybe "" (lookupAttr (unqual "value") (elAttribs el))}

-- Get the list of RawTransition from mxCell elements that have no "value" attribute but BOTH a "source" and a "target" attribute
getTransitions :: [Element] -> [RawTransition]
getTransitions = map toRawTransition . filter (\el -> isNothing (lookupAttr (unqual "value") (elAttribs el)) && isJust (lookupAttr (unqual "source") (elAttribs el)) && isJust (lookupAttr (unqual "target") (elAttribs el)))
  where
    toRawTransition el = RawTransition {rawSourceId = fromMaybe "" (lookupAttr (unqual "source") (elAttribs el)), rawTargetId = fromMaybe "" (lookupAttr (unqual "target") (elAttribs el))}

createStates :: String -> [RawState] -> [RawTransition] -> [State]
createStates startStateRawId rawStates rawTransitions =
  let idToName = [(rawId rs, rawName rs) | rs <- rawStates]
      lookupName rid = fromMaybe "" (lookup rid idToName)
      mkState rs =
        let trans = [lookupName (rawTargetId t) | t <- rawTransitions, rawSourceId t == rawId rs]
         in State {name = rawName rs, transitions = trans}
      states = map mkState rawStates
      (startStates, otherStates) = span ((== startStateRawId) . rawId) rawStates
      orderedStates = case startStates of
        (s : _) -> mkState s : [mkState rs | rs <- otherStates]
        [] -> states
   in orderedStates

parseXml :: String -> [State]
parseXml content =
  case parseXMLDoc content of
    Nothing -> []
    Just root ->
      let allElems = universe root
          mxCells = filter (\el -> qName (elName el) == "mxCell") allElems
          rawStates = getStates mxCells
          rawTransitions = getTransitions mxCells
          startId = getStartState mxCells
          states = createStates startId rawStates rawTransitions
       in states

-- Lowercase the first letter of a string
lowercaseFirst :: String -> String
lowercaseFirst [] = []
lowercaseFirst (x : xs) = toLower x : xs

-- Generate controller imports from a list of controller names
generateControllerImports :: [String] -> String
generateControllerImports controllerNames =
  let imports = map (\name -> "import Helpers.Controllers." ++ capitalize name) controllerNames
   in unlines imports

-- Generate decode statements for critical controllers
generateCriticalControllerDecodes :: [IB.RawControllerInputState] -> String
generateCriticalControllerDecodes controllers =
  let criticalControllers = filter IB.rawCritical controllers
      decodeStatements = map generateDecodeStatement criticalControllers
   in unlines decodeStatements
  where
    generateDecodeStatement controller =
      let controllerName = IB.rawControllerName controller
          capitalizedName = capitalize controllerName
       in "  let (" ++ controllerName ++ ", " ++ controllerName ++ "ErrFlag, " ++ controllerName ++ "DebugMsg) = decode" ++ capitalizedName ++ "State inputStr"

-- Generate default output type declarations using the generated default constructors
generateDefaultOutputTypes :: [OB.RawControllerOutputState] -> String
generateDefaultOutputTypes outputControllers =
  let defaultStatements = map generateDefaultStatement outputControllers
   in unlines defaultStatements
  where
    generateDefaultStatement controller =
      let controllerName = OB.rawOutputControllerName controller
          capitalizedName = capitalize controllerName
       in "  let " ++ controllerName ++ "Out = default" ++ capitalizedName

-- Generate OutputData field assignments
generateOutputDataFields :: [OB.RawControllerOutputState] -> String
generateOutputDataFields outputControllers =
  let fieldAssignments = map generateFieldAssignment outputControllers
   in intercalate ", " fieldAssignments
  where
    generateFieldAssignment controller =
      let controllerName = OB.rawOutputControllerName controller
       in controllerName ++ " = " ++ controllerName ++ "Out"

-- Generate input errors and debug messages tuples
generateInputErrorsDebugs :: [IB.RawControllerInputState] -> String
generateInputErrorsDebugs controllers =
  let criticalControllers = filter IB.rawCritical controllers
      errorTuples = map generateErrorTuple criticalControllers
   in intercalate ", " errorTuples
  where
    generateErrorTuple controller =
      let controllerName = IB.rawControllerName controller
       in "(\"" ++ controllerName ++ "\", " ++ controllerName ++ "ErrFlag, " ++ controllerName ++ "DebugMsg)"

-- Generate function input types for stateBehaviour and stateTransition
generateInputTypes :: [IB.RawControllerInputState] -> String
generateInputTypes controllers =
  let criticalControllers = filter IB.rawCritical controllers
      inputTypes = map (\controller -> capitalize (IB.rawControllerName controller) ++ "State") criticalControllers
   in intercalate " -> " inputTypes

-- Generate function input parameters for stateBehaviour and stateTransition
generateInputParameters :: [IB.RawControllerInputState] -> String
generateInputParameters controllers =
  let criticalControllers = filter IB.rawCritical controllers
      inputParams = map IB.rawControllerName criticalControllers
   in intercalate " " inputParams

-- Generate input variables for function calls
generateInputVariables :: [IB.RawControllerInputState] -> String
generateInputVariables controllers =
  let criticalControllers = filter IB.rawCritical controllers
      inputVars = map IB.rawControllerName criticalControllers
   in intercalate " " inputVars

-- Generate output type for stateBehaviour function
generateOutputType :: [OB.RawControllerOutputState] -> String
generateOutputType outputControllers =
  case outputControllers of
    [controller] -> capitalize (OB.rawOutputControllerName controller)  -- Remove "State" suffix
    _ -> "Turtlebot" -- Default fallback to actual output type

-- Generate output variable name
generateOutputVariable :: [OB.RawControllerOutputState] -> String
generateOutputVariable outputControllers =
  case outputControllers of
    [controller] -> OB.rawOutputControllerName controller ++ "Out"
    _ -> "turtlebotOut" -- Default fallback

-- Generate default output constructor
generateDefaultOutput :: [OB.RawControllerOutputState] -> String
generateDefaultOutput outputControllers =
  case outputControllers of
    [controller] -> "default" ++ capitalize (OB.rawOutputControllerName controller)
    _ -> "defaultTurtlebot" -- Default fallback

-- Generate SF input type (for single controller case, just the controller type)
generateSFInputType :: [IB.RawControllerInputState] -> String
generateSFInputType controllers =
  let criticalControllers = filter IB.rawCritical controllers
   in case criticalControllers of
        [controller] -> capitalize (IB.rawControllerName controller) ++ "State"
        _ -> "TurtlebotState" -- Default fallback

-- Generate SF input parameter (for single controller case, just the controller name)
generateSFInputParameter :: [IB.RawControllerInputState] -> String
generateSFInputParameter controllers =
  let criticalControllers = filter IB.rawCritical controllers
   in case criticalControllers of
        [controller] -> IB.rawControllerName controller
        _ -> "turtlebot" -- Default fallback

-- Generate SF input variable for function calls (same as parameter)
generateSFInputVariable :: [IB.RawControllerInputState] -> String
generateSFInputVariable = generateSFInputParameter

-- Generate SF input variable for analyzer function calls
generateSFInputVariableAnalyzer :: [IB.RawControllerInputState] -> String
generateSFInputVariableAnalyzer = generateSFInputParameter

-- Generate control logic comments
generateControlLogicComments :: [IB.RawControllerInputState] -> [OB.RawControllerOutputState] -> String
generateControlLogicComments inputControllers outputControllers =
  let outputVar = generateOutputVariable outputControllers
      criticalInputs = filter IB.rawCritical inputControllers
      outputFields = case outputControllers of
        [controller] -> OB.rawOutputs controller
        _ -> [] -- Default fallback
      
      -- Generate example field assignments
      fieldExamples = case outputFields of
        (field1:field2:_) -> 
          "      -- " ++ outputVar ++ "' = " ++ outputVar ++ " { \n" ++
          "      --   " ++ OB.rawOutputName field1 ++ " = if (value " ++ (head (map IB.rawControllerName criticalInputs)) ++ ") > 0.5 then 1.0 else 0.0,\n" ++
          "      --   " ++ OB.rawOutputName field2 ++ " = if (value " ++ (head (map IB.rawControllerName criticalInputs)) ++ ") > 0.5 then 1.0 else 0.0 \n" ++
          "      -- }"
        [field] ->
          "      -- " ++ outputVar ++ "' = " ++ outputVar ++ " { " ++ OB.rawOutputName field ++ " = newValue }"
        _ ->
          "      -- " ++ outputVar ++ "' = " ++ outputVar ++ " { field1 = value1, field2 = value2 }"
      
      -- Generate sensor access examples
      sensorExamples = case criticalInputs of
        (input:_) -> 
          "      -- \n" ++
          "      -- Access input sensor data like:\n" ++
          "      -- - " ++ IB.rawControllerName input ++ " sensor: (value " ++ IB.rawControllerName input ++ ") gives you the sensor reading\n" ++
          "      -- - " ++ (if null criticalInputs then "turtlebot" else head (map IB.rawControllerName criticalInputs)) ++ " state: use existing parameter for feedback control"
        _ -> 
          "      -- \n" ++
          "      -- Access input sensor data from the function parameters"
   in fieldExamples ++ sensorExamples

-- Generate decode statements for analyzer
generateCriticalControllerDecodesAnalyzer :: [IB.RawControllerInputState] -> String
generateCriticalControllerDecodesAnalyzer controllers =
  let criticalControllers = filter IB.rawCritical controllers
      decodeStatements = map generateDecodeStatement criticalControllers
   in unlines decodeStatements
  where
    generateDecodeStatement controller =
      let controllerName = IB.rawControllerName controller
          capitalizedName = capitalize controllerName
       in "  let (" ++ controllerName ++ ", " ++ controllerName ++ "ErrFlag, " ++ controllerName ++ "DebugMsg) = decode" ++ capitalizedName ++ "State sfInput"

-- Generate input variables for analyzer function calls
generateInputVariablesAnalyzer :: [IB.RawControllerInputState] -> String
generateInputVariablesAnalyzer controllers =
  let criticalControllers = filter IB.rawCritical controllers
      inputVars = map IB.rawControllerName criticalControllers
   in intercalate " " inputVars

-- Generate transition logic comments
generateTransitionLogicComments :: [IB.RawControllerInputState] -> String
generateTransitionLogicComments inputControllers =
  let criticalInputs = filter IB.rawCritical inputControllers
      exampleCondition = case criticalInputs of
        (input:_) -> 
          "      -- shouldSwitch = (value " ++ IB.rawControllerName input ++ ") > 0.8  -- Switch when sensor reading is high\n" ++
          "      -- targetState = if (value " ++ IB.rawControllerName input ++ ") > 0.8 then \"FastState\" \n" ++
          "      --              else if (value " ++ IB.rawControllerName input ++ ") < 0.2 then \"SlowState\"\n" ++
          "      --              else \"IdleState\"\n" ++
          "      --\n" ++
          "      -- Access input data:\n" ++
          "      -- - (value " ++ IB.rawControllerName input ++ "): Get the sensor reading from " ++ IB.rawControllerName input ++ " controller"
        _ -> 
          "      -- shouldSwitch = someCondition  -- Define your switching condition\n" ++
          "      -- targetState = \"TargetStateName\"  -- Define target state"
      
      additionalInputs = case drop 1 criticalInputs of
        (input2:_) -> "\n      -- - " ++ IB.rawControllerName input2 ++ ": Access " ++ IB.rawControllerName input2 ++ " state for feedback-based decisions"
        _ -> case criticalInputs of
          (_:_) -> "\n      -- - turtlebot: Access previous turtlebot state for feedback-based decisions"
          _ -> ""
   in exampleCondition ++ additionalInputs

-- Generate the ErrorState file using the errorstate template (only if it doesn't exist)
writeErrorStateFile :: [String] -> [IB.RawControllerInputState] -> [OB.RawControllerOutputState] -> IO ()
writeErrorStateFile controllerNames controllers outputControllers = do
  let stateName = "Error" :: String
      moduleName = "Helpers.States.ErrorState" :: String
      fileName = "FsmRunner/src/Helpers/States/errorState.hs" :: String
      controllerImports = generateControllerImports controllerNames
      criticalDecodes = generateCriticalControllerDecodes controllers
      defaultOutputTypes = generateDefaultOutputTypes outputControllers
      outputDataFields = generateOutputDataFields outputControllers
      inputErrorsDebugs = generateInputErrorsDebugs controllers
  
  fileExists <- doesFileExist fileName
  if fileExists
    then putStrLn $ "Skipped " ++ fileName ++ " (already exists)"
    else do
      eTemplate <- Mustache.automaticCompile ["./FsmBuilder/templates"] "errorstate.mustache"
      case eTemplate of
        Left err -> putStrLn $ "Template parse error: " ++ show err
        Right template -> do
          let context =
                Aeson.object
                  [ "stateName" Aeson..= stateName,
                    "moduleName" Aeson..= moduleName,
                    "import_controllers" Aeson..= controllerImports,
                    "decode_critical_controllers" Aeson..= criticalDecodes,
                    "default_outputtypes" Aeson..= defaultOutputTypes,
                    "outputdata" Aeson..= outputDataFields,
                    "input_errors_debugs" Aeson..= inputErrorsDebugs
                  ]
              rendered = Mustache.substitute template context
          -- replace HTML entities with real characters and write
          let fixed = T.replace "&quot;" "\"" . T.replace "&gt;" ">" . T.replace "&#39;" "'" $ rendered
          writeFile fileName (T.unpack fixed)
          putStrLn $ "Wrote " ++ fileName

-- Generate a state file from a State datatype using the template (only if it doesn't exist)
writeStateFile :: [String] -> [IB.RawControllerInputState] -> [OB.RawControllerOutputState] -> State -> IO ()
writeStateFile controllerNames controllers outputControllers st = do
  let stateName = capitalize (name st)
      moduleName = "Helpers.States." ++ stateName
      fileName = "FsmRunner/src/Helpers/States/" ++ lowercaseFirst stateName ++ "State.hs"
      controllerImports = generateControllerImports controllerNames
      criticalDecodes = generateCriticalControllerDecodes controllers
      defaultOutputTypes = generateDefaultOutputTypes outputControllers
      outputDataFields = generateOutputDataFields outputControllers
      inputErrorsDebugs = generateInputErrorsDebugs controllers
      -- Original template variables (kept for compatibility)
      inputTypes = generateInputTypes controllers
      inputParameters = generateInputParameters controllers
      inputVariables = generateInputVariables controllers
      outputType = generateOutputType outputControllers
      outputVariable = generateOutputVariable outputControllers
      defaultOutput = generateDefaultOutput outputControllers
      controlLogicComments = generateControlLogicComments controllers outputControllers
      criticalDecodesAnalyzer = generateCriticalControllerDecodesAnalyzer controllers
      inputVariablesAnalyzer = generateInputVariablesAnalyzer controllers
      transitionLogicComments = generateTransitionLogicComments controllers
      -- New SF template variables
      sfInputType = generateSFInputType controllers
      sfInputParameter = generateSFInputParameter controllers
      sfInputVariable = generateSFInputVariable controllers
      sfInputVariableAnalyzer = generateSFInputVariableAnalyzer controllers
  
  fileExists <- doesFileExist fileName
  if fileExists
    then putStrLn $ "Skipped " ++ fileName ++ " (already exists)"
    else do
      eTemplate <- Mustache.automaticCompile ["./FsmBuilder/templates"] "state.mustache"
      case eTemplate of
        Left err -> putStrLn $ "Template parse error: " ++ show err
        Right template -> do
          let context =
                Aeson.object
                  [ "stateName" Aeson..= stateName,
                    "stateNameLower" Aeson..= lowercaseFirst stateName,
                    "moduleName" Aeson..= moduleName,
                    "import_controllers" Aeson..= controllerImports,
                    "decode_critical_controllers" Aeson..= criticalDecodes,
                    "default_outputtypes" Aeson..= defaultOutputTypes,
                    "outputdata" Aeson..= outputDataFields,
                    "input_errors_debugs" Aeson..= inputErrorsDebugs,
                    -- Original template variables (kept for compatibility)
                    "input_types" Aeson..= inputTypes,
                    "input_parameters" Aeson..= inputParameters,
                    "input_variables" Aeson..= inputVariables,
                    "output_type" Aeson..= outputType,
                    "output_variable" Aeson..= outputVariable,
                    "default_output" Aeson..= defaultOutput,
                    "control_logic_comments" Aeson..= controlLogicComments,
                    "decode_critical_controllers_analyzer" Aeson..= criticalDecodesAnalyzer,
                    "input_variables_analyzer" Aeson..= inputVariablesAnalyzer,
                    "transition_logic_comments" Aeson..= transitionLogicComments,
                    -- New SF template variables
                    "sf_input_type" Aeson..= sfInputType,
                    "sf_input_parameter" Aeson..= sfInputParameter,
                    "sf_input_variable" Aeson..= sfInputVariable,
                    "sf_input_variable_analyzer" Aeson..= sfInputVariableAnalyzer
                  ]
              rendered = Mustache.substitute template context
          -- replace HTML entities with real characters and write
          let fixed = T.replace "&quot;" "\"" . T.replace "&gt;" ">" . T.replace "&#39;" "'" $ rendered
          writeFile fileName (T.unpack fixed)

-- Generate import and mapping lines for a list of State names
createMappingEntries :: [String] -> String
createMappingEntries stateNames =
  let cap s = toUpper (head s) : tail s
      imp s = "import Helpers.States." ++ cap s ++ "State"
      mapLine s = "  | eventOutput == \"" ++ s ++ "State\"  = kSwitch " ++ s ++ "StateSF analyzer" ++ cap s ++ "State mapping"
      imports = unlines $ map imp stateNames
      mappings = unlines $ map mapLine stateNames
   in imports ++ "\n" ++ "mapping :: SF String OutputState -> String -> SF String OutputState\nmapping startingSF eventOutput\n" ++ mappings ++ "  | otherwise = errorStateSF\n"

-- Generate a control.hs file from a list of State names
writeControllogicFile :: [String] -> [IB.RawControllerInputState] -> [OB.RawControllerOutputState] -> [State] -> IO ()
writeControllogicFile controllerNames controllers outputControllers states = do
  let stateNames = map name states
      startState = capitalize (head stateNames)
      startStateLower = lowercaseFirst (head stateNames)
      mappingEntries = createMappingEntries (map lowercaseFirst stateNames)
      controllerImports = generateControllerImports controllerNames

  eTemplate <- Mustache.automaticCompile ["./FsmBuilder/templates"] "controllogic.mustache"
  case eTemplate of
    Left err -> putStrLn $ "Template parse error: " ++ show err
    Right template -> do
      let context =
            Aeson.object
              [ "import_controllers" Aeson..= controllerImports,
                "mappingEntries" Aeson..= mappingEntries,
                "startState" Aeson..= startState,
                "startStateLower" Aeson..= startStateLower
              ]
          rendered = Mustache.substitute template context
      writeFile "FsmRunner/src/controllogic.hs" (T.unpack rendered)
      putStrLn "Wrote FsmRunner/src/controllogic.hs"

-- Generate diagramData JS code for Mermaid from a list of State
createDiagramDataJS :: [State] -> String
createDiagramDataJS states =
  let stateNames = map name states
      associations = concatMap (\st -> map (\tr -> [name st, tr]) (transitions st)) states
      jsStates = show stateNames
      jsAssociations = show associations
      startState = if null stateNames then "" else head stateNames
   in unlines
        [ "let diagramData = {",
          "  \"classes\": " ++ jsStates ++ ",",
          "  \"associations\": " ++ jsAssociations ++ ",",
          "};",
          "let activeClass = \"" ++ startState ++ "\";"
        ]

-- Manage States directory: only remove files that are no longer needed
manageStatesDirectory :: [String] -> IO ()
manageStatesDirectory neededStateNames = do
  let outDir = "FsmRunner/src/Helpers/States"
  createDirectoryIfMissing True outDir
  allItems <- listDirectory outDir
  
  -- Filter to only get files (not directories)
  files <- filterM (\item -> do
    let fullPath = outDir </> item
    isFile <- doesFileExist fullPath
    return isFile) allItems
  
  -- Generate expected filenames for needed states (including ErrorState)
  let expectedFiles = map (\name -> lowercaseFirst (capitalize name) ++ "State.hs") neededStateNames ++ ["errorState.hs"]
  
  -- Files to keep: expected files and .bk files
  let filesToKeep = filter (\f -> f `elem` expectedFiles || takeExtension f == ".bk") files
  let filesToDelete = filter (\f -> f `notElem` expectedFiles && takeExtension f /= ".bk") files
  
  -- Remove only unnecessary files
  mapM_ (\f -> removeFile (outDir </> f)) filesToDelete
  
  putStrLn $ "Managed States directory: kept " ++ show (length filesToKeep) ++ " files, removed " ++ show (length filesToDelete) ++ " unnecessary files"

main :: [String] -> [IB.RawControllerInputState] -> [OB.RawControllerOutputState] -> [State] -> IO ()
main controllerNames controllers outputControllers states = do
  let stateNames = map name states
  manageStatesDirectory stateNames
  mapM_ (writeStateFile controllerNames controllers outputControllers) states
  writeErrorStateFile controllerNames controllers outputControllers
  writeControllogicFile controllerNames controllers outputControllers states
  MB.main controllers outputControllers stateNames
  putStrLn "FSM files generated successfully."
