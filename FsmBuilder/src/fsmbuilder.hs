{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module FsmBuilder where

import Data.Aeson qualified as Aeson
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text qualified as T
import InputBuilder qualified as IB
import MixedBuilder qualified as MB
import OutputBuilder qualified as OB
import RfrUtilities (capitalize)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
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

-- Generate the ErrorState file using the errorstate template
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
      -- replace HTML entity &quot; with real double-quote and write
      let fixed = T.replace "&quot;" "\"" rendered
      writeFile fileName (T.unpack fixed)
      putStrLn $ "Wrote " ++ fileName

-- Generate a state file from a State datatype using the template TODO: generate the import Inputs based on YML file
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
                "input_errors_debugs" Aeson..= inputErrorsDebugs
              ]
          rendered = Mustache.substitute template context
      -- replace HTML entity &quot; with real double-quote and write
      let fixed = T.replace "&quot;" "\"" rendered
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

-- Write fsm.html using the template and createDiagramDataJS output
writeFSMHtmlFile :: [State] -> IO ()
writeFSMHtmlFile states = do
  let diagramDataJS = createDiagramDataJS states
      htmlContent =
        unlines
          [ "<!DOCTYPE html>",
            "<html lang=\"en\">",
            "",
            "<head>",
            "  <meta charset=\"UTF-8\">",
            "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
            "  <title>Mermaid & MQTT Client</title>",
            "",
            "  <script src=\"https://unpkg.com/mqtt/dist/mqtt.min.js\"></script>",
            "  <script type=\"module\">",
            "    import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';",
            "    mermaid.initialize({ startOnLoad: true });",
            "",
            "    const brokerUrl = \"ws://localhost:9001\"; // Aanpassen indien nodig",
            "    const topicHighlight = \"update/robot_state\"; // Highlight klasse naam",
            "",
            "    const client = mqtt.connect(brokerUrl);",
            "",
            diagramDataJS,
            "",
            "    client.on(\"connect\", () => {",
            "      console.log(\"[DEBUG] Verbonden met MQTT broker\");",
            "",
            "      client.subscribe(topicClasses, (err) => {",
            "        if (!err) {",
            "          console.log(`[DEBUG] Geabonneerd op ${topicClasses}`);",
            "          client.publish(topicClasses, \"REQUEST_UPDATE\");",
            "        }",
            "      });",
            "",
            "      client.subscribe(topicHighlight, (err) => {",
            "        if (!err) {",
            "          console.log(`[DEBUG] Geabonneerd op ${topicHighlight}`);",
            "          client.publish(topicHighlight, \"REQUEST_UPDATE\");",
            "        }",
            "      });",
            "    });",
            "",
            "    client.on(\"message\", (topic, message) => {",
            "      console.log(`[DEBUG] Bericht ontvangen op ${topic}: ${message.toString()}`);",
            "      try {",
            "        const data = JSON.parse(message.toString());",
            "",
            "        if (topic === topicClasses) {",
            "          diagramData = data;",
            "          updateDiagram();",
            "        } else if (topic === topicHighlight) {",
            "          activeClass = data.activeClass || null; // Verwacht: { \"activeClass\": \"ClassName\" }",
            "          if (!diagramData.classes.includes(activeClass)) {",
            "            activeClass = null;",
            "          }",
            "          updateDiagram();",
            "        } else {",
            "          console.warn(`[DEBUG] Onbekend topic: ${topic}`);",
            "        }",
            "",
            "      } catch (err) {",
            "        console.error(\"[DEBUG] Fout bij parsen van bericht:\", err);",
            "      }",
            "    });",
            "",
            "    function updateDiagram() {",
            "      const container = document.getElementById(\"mermaid-container\");",
            "      if (!diagramData) return;",
            "",
            "      let diagram = \"graph LR\\n\";",
            "      diagramData.classes.forEach(cls => {",
            "        if (cls === activeClass) {",
            "          diagram += `${cls}([\"${cls}\"]):::highlight\\n`;",
            "        } else {",
            "          diagram += `${cls}(${cls})\\n`;",
            "        }",
            "      });",
            "      diagramData.associations.forEach(([c1, c2]) => {",
            "        diagram += `${c1} --> ${c2}\\n`;",
            "      });",
            "",
            "      container.classList.remove(\"fade-in\");",
            "      container.innerHTML = `<pre class=\"mermaid\">${diagram}</pre>`;",
            "",
            "      mermaid.init(undefined, container.querySelector(\".mermaid\"));",
            "      setTimeout(() => {",
            "        container.classList.add(\"fade-in\");",
            "      }, 10);",
            "    }",
            "",
            "    document.addEventListener(\"DOMContentLoaded\", () => {",
            "      updateDiagram();",
            "    });",
            "",
            "  </script>",
            "",
            "  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.css\">",
            "",
            "  <style>",
            "    #mermaid-container {",
            "      transition: opacity 0.5s ease;",
            "      opacity: 1;",
            "      min-height: 200px;",
            "    }",
            "",
            "    #mermaid-container.fade-in {",
            "      opacity: 1;",
            "    }",
            "",
            "    #mermaid-container:not(.fade-in) {",
            "      opacity: 0;",
            "    }",
            "",
            "    /* Mermaid custom styling voor highlighten */",
            "    .mermaid .highlight>rect {",
            "      fill: #ffeb3b !important;",
            "      /* Gele kleur */",
            "      stroke: #f57f17 !important;",
            "      stroke-width: 3px;",
            "    }",
            "  </style>",
            "</head>",
            "",
            "<body>",
            "  <h1>Mermaid & MQTT Client</h1>",
            "  <div id=\"mermaid-container\" class=\"fade-in\">Loading...</div>",
            "</body>",
            "",
            "</html>"
          ]
  writeFile "FsmRunner/src/fsm.html" htmlContent

-- Clean up States directory (except .bk files)
cleanStatesDirectory :: IO ()
cleanStatesDirectory = do
  let outDir = "FsmRunner/src/Helpers/States"
  createDirectoryIfMissing True outDir
  files <- listDirectory outDir
  let filesToDelete = filter (\f -> takeExtension f /= ".bk") files
  mapM_ (\f -> removeFile (outDir </> f)) filesToDelete
  putStrLn $ "Cleaned States directory, preserved " ++ show (length files - length filesToDelete) ++ " .bk files"

main :: [String] -> [IB.RawControllerInputState] -> [OB.RawControllerOutputState] -> [State] -> IO ()
main controllerNames controllers outputControllers states = do
  let stateNames = map name states
  cleanStatesDirectory
  mapM_ (writeStateFile controllerNames controllers outputControllers) states
  writeErrorStateFile controllerNames controllers outputControllers
  writeControllogicFile controllerNames controllers outputControllers states
  MB.main controllers outputControllers stateNames
  -- writeFSMHtmlFile states TODO FIXME
  putStrLn "FSM files generated successfully."
