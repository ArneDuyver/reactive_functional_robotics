{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module MixedBuilder where

import Data.Aeson qualified as Aeson
import Data.List (intercalate)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import InputBuilder qualified as IB
import OutputBuilder qualified as OB
import RfrUtilities (capitalize)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Mustache qualified as Mustache

-- Generate controller module imports for cabal file
generateControllerModules :: [IB.RawControllerInputState] -> [OB.RawControllerOutputState] -> String
generateControllerModules inputControllers outputControllers =
  let inputModules =
        map
          ( \controller ->
              "                    , Helpers.Controllers." ++ capitalize (IB.rawControllerName controller)
          )
          inputControllers
      outputModules =
        map
          ( \controller ->
              "                    , Helpers.Controllers." ++ capitalize (OB.rawOutputControllerName controller)
          )
          outputControllers
      -- Remove duplicates by converting to a set-like structure
      allModules = inputModules ++ outputModules
      uniqueModules = removeDuplicates allModules
   in intercalate "\n" uniqueModules
  where
    removeDuplicates [] = []
    removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

-- Generate state module imports for cabal file
generateStateModules :: [String] -> String
generateStateModules stateNames =
  let stateModules =
        map
          ( \stateName ->
              "                    , Helpers.States." ++ capitalize stateName ++ "State"
          )
          stateNames
   in intercalate "\n" stateModules

-- Generate the fsmrunner.cabal file
generateCabalFile :: [IB.RawControllerInputState] -> [OB.RawControllerOutputState] -> [String] -> IO ()
generateCabalFile inputControllers outputControllers stateNames = do
  let controllerModules = generateControllerModules inputControllers outputControllers
      stateModules = generateStateModules stateNames

  -- Load template and render
  eTemplate <- Mustache.automaticCompile ["./FsmBuilder/templates"] "fsmrunner-cabal.mustache"
  case eTemplate of
    Left err -> putStrLn $ "Template parse error: " ++ show err
    Right template -> do
      let context =
            Aeson.object
              [ "controller_modules" Aeson..= controllerModules,
                "state_modules" Aeson..= stateModules
              ]
      let rendered = Mustache.substitute template context
      let outDir = "FsmRunner"
      let outFile = outDir </> "fsmrunner.cabal"
      createDirectoryIfMissing True outDir
      -- Replace HTML entity &quot; with real double-quote and write
      let fixed = T.replace "&quot;" "\"" rendered
      TIO.writeFile outFile fixed
      putStrLn $ "Wrote " ++ outFile

main :: [IB.RawControllerInputState] -> [OB.RawControllerOutputState] -> [String] -> IO ()
main inputControllers outputControllers stateNames =
  generateCabalFile inputControllers outputControllers stateNames