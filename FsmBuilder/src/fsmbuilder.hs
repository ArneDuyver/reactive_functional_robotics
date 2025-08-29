{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module FsmBuilder where

import Text.XML.Light
import Text.XML.Light.Output (showElement)
import System.Environment (getArgs)
import Data.Maybe (isJust, isNothing, fromMaybe)
import System.Directory (createDirectoryIfMissing)
import Data.Char (toUpper, toLower)
import qualified Text.Mustache as Mustache
import qualified Data.Aeson as Aeson

-- TODO: do not make it overwrite existing files!!!

-- Intermediate representation for parsing
data RawState = RawState
  { rawId :: String
  , rawName :: String
  } deriving (Show, Eq)

-- Intermediate representation for parsing
data State = State
  { name :: String
  , transitions :: [String]
  } deriving (Show, Eq)

-- Intermediate representation for parsing
data RawTransition = RawTransition
  { rawSourceId :: String
  , rawTargetId :: String
  } deriving (Show, Eq)

-- Recursively get all elements in the XML tree (elChildren: returns a list of child Elements for a given XML Element. concatMap: apply universe to every element and concatenate into one list)
universe :: Element -> [Element]
universe e = e : concatMap universe (elChildren e)

-- Get the "target" attribute from the first mxCell element that has BOTH a "value" and a "target" attribute
getStartState :: [Element] -> String
getStartState els = case filter (\el -> isJust (lookupAttr (unqual "value") (elAttribs el)) && isJust (lookupAttr (unqual "target") (elAttribs el))) els of
  (el:_) -> fromMaybe "" (lookupAttr (unqual "target") (elAttribs el))
  []     -> ""

-- Get the list of RawState from mxCell elements that have a "value" attribute and no "target" attribute
getStates :: [Element] -> [RawState]
getStates = map toRawState . filter (\el -> isJust (lookupAttr (unqual "value") (elAttribs el)) && isNothing (lookupAttr (unqual "target") (elAttribs el)))
  where
    toRawState el = RawState { rawId = fromMaybe "" (lookupAttr (unqual "id") (elAttribs el)), rawName = fromMaybe "" (lookupAttr (unqual "value") (elAttribs el)) }

-- Get the list of RawTransition from mxCell elements that have no "value" attribute but BOTH a "source" and a "target" attribute
getTransitions :: [Element] -> [RawTransition]
getTransitions = map toRawTransition . filter (\el -> isNothing (lookupAttr (unqual "value") (elAttribs el)) && isJust (lookupAttr (unqual "source") (elAttribs el)) && isJust (lookupAttr (unqual "target") (elAttribs el)))
  where
    toRawTransition el = RawTransition { rawSourceId = fromMaybe "" (lookupAttr (unqual "source") (elAttribs el)), rawTargetId = fromMaybe "" (lookupAttr (unqual "target") (elAttribs el)) }

createStates :: String -> [RawState] -> [RawTransition] -> [State]
createStates startStateRawId rawStates rawTransitions =
  let idToName = [(rawId rs, rawName rs) | rs <- rawStates]
      lookupName rid = fromMaybe "" (lookup rid idToName)
      mkState rs =
        let trans = [lookupName (rawTargetId t) | t <- rawTransitions, rawSourceId t == rawId rs]
        in State { name = rawName rs, transitions = trans }
      states = map mkState rawStates
      (startStates, otherStates) = span ((== startStateRawId) . rawId) rawStates
      orderedStates = case startStates of
        (s:_) -> mkState s : [mkState rs | rs <- otherStates]
        []    -> states
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

-- Capitalize the first letter of a string
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- Lowercase the first letter of a string
lowercaseFirst :: String -> String
lowercaseFirst [] = []
lowercaseFirst (x:xs) = toLower x : xs

-- Generate a state file from a State datatype using the template TODO: generate the import Inputs based on YML file
writeStateFile :: State -> IO ()
writeStateFile st = do
  let stateName = capitalize (name st)
      moduleName = "Helpers.States." ++ stateName
      fileName = "FsmRunner/src/Helpers/States/" ++ lowercaseFirst stateName ++ "State.hs"
  eTemplate <- Mustache.automaticCompile ["./FsmBuilder/templates"] "state.mustache"
  case eTemplate of
    Left err -> putStrLn $ "Template parse error: " ++ show err
    Right template -> do
      let context = Aeson.object
            [ "stateName" Aeson..= stateName
            , "stateNameLower" Aeson..= lowercaseFirst stateName
            , "moduleName" Aeson..= moduleName
            ]
          rendered = Mustache.substitute template context
      createDirectoryIfMissing True "states"
      writeFile fileName (show rendered)

-- Generate import and mapping lines for a list of State names
createMappingEntries :: [String] -> String
createMappingEntries stateNames =
  let cap s = toUpper (head s) : tail s
      imp s = "import Helpers.States." ++ cap s ++ "State"
      mapLine s = "  | eventOutput == \"" ++ s ++ "State\"  = kSwitch " ++ s ++ "StateSF analyzer" ++ cap s ++ "State mapping"
      imports = unlines $ map imp stateNames
      mappings = unlines $ map mapLine stateNames
  in imports ++ "\n" ++ "mapping :: SF String (Double, Double, Bool, String) -> String -> SF String (Double, Double, Bool, String)\nmapping startingSF eventOutput\n" ++ mappings ++ "  | otherwise = errorStateSF\n"

-- Generate a control.hs file from a list of State names
writeControllogicFile :: [State] -> IO ()
writeControllogicFile states = do
  let stateNames = map name states
      startState = capitalize (head stateNames)
      mappingEntries = createMappingEntries (map lowercaseFirst stateNames)
      content = unlines
        [ "{-# LANGUAGE Arrows #-}"
        , ""
        , "module Controllogic where"
        , ""
        , "import Control.Concurrent"
        , "import FRP.Yampa"
        , "import Helpers.Turtlebot"
        , "import Helpers.KeyboardController"
        , "import Helpers.YampaHelper"
        , "import Data.Bool (Bool (False))"
        , "import Numeric (showFFloat)"
        , ""
        , mappingEntries
        , "-- MAIN"
        , "mainSF :: SF String (Double, Double, Bool, String)"
        , "mainSF = kSwitch " ++ lowercaseFirst startState ++ "SF analyzer" ++ startState ++ "State mapping"
        ]
  writeFile "FsmRunner/src/controllogic.hs" content

-- Generate diagramData JS code for Mermaid from a list of State
createDiagramDataJS :: [State] -> String
createDiagramDataJS states =
  let stateNames = map name states
      associations = concatMap (\st -> map (\tr -> [name st, tr]) (transitions st)) states
      jsStates = show stateNames
      jsAssociations = show associations
      startState = if null stateNames then "" else head stateNames
  in unlines
    [ "let diagramData = {"
    , "  \"classes\": " ++ jsStates ++ ","
    , "  \"associations\": " ++ jsAssociations ++ ","
    , "};"
    , "let activeClass = \"" ++ startState ++ "\";"
    ]

-- Write fsm.html using the template and createDiagramDataJS output
writeFSMHtmlFile :: [State] -> IO ()
writeFSMHtmlFile states = do
  let diagramDataJS = createDiagramDataJS states
      htmlContent = unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\">"
        , ""
        , "<head>"
        , "  <meta charset=\"UTF-8\">"
        , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        , "  <title>Mermaid & MQTT Client</title>"
        , ""
        , "  <script src=\"https://unpkg.com/mqtt/dist/mqtt.min.js\"></script>"
        , "  <script type=\"module\">"
        , "    import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';"
        , "    mermaid.initialize({ startOnLoad: true });"
        , ""
        , "    const brokerUrl = \"ws://localhost:9001\"; // Aanpassen indien nodig"
        , "    const topicHighlight = \"update/robot_state\"; // Highlight klasse naam"
        , ""
        , "    const client = mqtt.connect(brokerUrl);"
        , ""
        , diagramDataJS
        , ""
        , "    client.on(\"connect\", () => {"
        , "      console.log(\"[DEBUG] Verbonden met MQTT broker\");"
        , ""
        , "      client.subscribe(topicClasses, (err) => {"
        , "        if (!err) {"
        , "          console.log(`[DEBUG] Geabonneerd op ${topicClasses}`);"
        , "          client.publish(topicClasses, \"REQUEST_UPDATE\");"
        , "        }"
        , "      });"
        , ""
        , "      client.subscribe(topicHighlight, (err) => {"
        , "        if (!err) {"
        , "          console.log(`[DEBUG] Geabonneerd op ${topicHighlight}`);"
        , "          client.publish(topicHighlight, \"REQUEST_UPDATE\");"
        , "        }"
        , "      });"
        , "    });"
        , ""
        , "    client.on(\"message\", (topic, message) => {"
        , "      console.log(`[DEBUG] Bericht ontvangen op ${topic}: ${message.toString()}`);"
        , "      try {"
        , "        const data = JSON.parse(message.toString());"
        , ""
        , "        if (topic === topicClasses) {"
        , "          diagramData = data;"
        , "          updateDiagram();"
        , "        } else if (topic === topicHighlight) {"
        , "          activeClass = data.activeClass || null; // Verwacht: { \"activeClass\": \"ClassName\" }"
        , "          if (!diagramData.classes.includes(activeClass)) {"
        , "            activeClass = null;"
        , "          }"
        , "          updateDiagram();"
        , "        } else {"
        , "          console.warn(`[DEBUG] Onbekend topic: ${topic}`);"
        , "        }"
        , ""
        , "      } catch (err) {"
        , "        console.error(\"[DEBUG] Fout bij parsen van bericht:\", err);"
        , "      }"
        , "    });"
        , ""
        , "    function updateDiagram() {"
        , "      const container = document.getElementById(\"mermaid-container\");"
        , "      if (!diagramData) return;"
        , ""
        , "      let diagram = \"graph LR\\n\";"
        , "      diagramData.classes.forEach(cls => {"
        , "        if (cls === activeClass) {"
        , "          diagram += `${cls}([\"${cls}\"]):::highlight\\n`;"
        , "        } else {"
        , "          diagram += `${cls}(${cls})\\n`;"
        , "        }"
        , "      });"
        , "      diagramData.associations.forEach(([c1, c2]) => {"
        , "        diagram += `${c1} --> ${c2}\\n`;"
        , "      });"
        , ""
        , "      container.classList.remove(\"fade-in\");"
        , "      container.innerHTML = `<pre class=\"mermaid\">${diagram}</pre>`;"
        , ""
        , "      mermaid.init(undefined, container.querySelector(\".mermaid\"));"
        , "      setTimeout(() => {"
        , "        container.classList.add(\"fade-in\");"
        , "      }, 10);"
        , "    }"
        , ""
        , "    document.addEventListener(\"DOMContentLoaded\", () => {"
        , "      updateDiagram();"
        , "    });"
        , ""
        , "  </script>"
        , ""
        , "  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.css\">"
        , ""
        , "  <style>"
        , "    #mermaid-container {"
        , "      transition: opacity 0.5s ease;"
        , "      opacity: 1;"
        , "      min-height: 200px;"
        , "    }"
        , ""
        , "    #mermaid-container.fade-in {"
        , "      opacity: 1;"
        , "    }"
        , ""
        , "    #mermaid-container:not(.fade-in) {"
        , "      opacity: 0;"
        , "    }"
        , ""
        , "    /* Mermaid custom styling voor highlighten */"
        , "    .mermaid .highlight>rect {"
        , "      fill: #ffeb3b !important;"
        , "      /* Gele kleur */"
        , "      stroke: #f57f17 !important;"
        , "      stroke-width: 3px;"
        , "    }"
        , "  </style>"
        , "</head>"
        , ""
        , "<body>"
        , "  <h1>Mermaid & MQTT Client</h1>"
        , "  <div id=\"mermaid-container\" class=\"fade-in\">Loading...</div>"
        , "</body>"
        , ""
        , "</html>"
        ]
  writeFile "FsmRunner/src/fsm.html" htmlContent

