module Main where

import FsmFrontend
import FsmFrontendLinux
import System.Environment (getArgs)
import Data.List (isPrefixOf)

-- Parse command line arguments
parseArgs :: [String] -> String
parseArgs [] = "windows"  -- Default OS
parseArgs args = 
  case filter ("--os=" `isPrefixOf`) args of
    (arg:_) -> drop 5 arg  -- Drop "--os=" to get the value
    []      -> "windows"   -- Default if no --os specified

main :: IO ()
main = do
  args <- getArgs
  let os = parseArgs args
  
  case os of
    "windows" -> startFrontend
    "linux"   -> startFrontendLinux
    other     -> putStrLn $ "Unknown OS option: " ++ other ++ "\nSupported options: windows, linux"
