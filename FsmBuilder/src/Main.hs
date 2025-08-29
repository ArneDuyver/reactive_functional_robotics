module Main where

import FsmBuilder

main :: IO ()
main = do
  -- TODO: give xml filename as an argument with main
  let filename = "config/fsm.xml"
  content <- readFile filename
  let states = parseXml content
  mapM_ putStrLn (map show states)
  mapM_ writeStateFile states
  writeControllogicFile states
  writeFSMHtmlFile states
  putStrLn ("Build: program from " ++ filename)