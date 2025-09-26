{-# LANGUAGE Arrows #-}

module Helpers.States.SimpleState where

import Control.Concurrent
import FRP.Yampa
import Helpers.Controllers.OutputState
import Helpers.Controllers.SimpleTwo
import Helpers.Controllers.Turtlebot
import Helpers.YampaHelper

simpleStateSF :: SF String OutputState
simpleStateSF = proc inputStr -> do
  -- Decode inputs for string
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr

  -- Create default types for Output
  let turtlebotOut = defaultTurtlebot

  -- #### Control logic CHANGE THE DEFAULT OUTPUT VALUES TO THE DESIRED VALUE ####

  -- #### END: Control logic ####

  -- Create OutputData with state name
  let outputData = OutputData {turtlebot = turtlebotOut, state = "Simple"}
  -- Create the error string
  let (errFlag, debugMsg) = createErrFlagAndDebugMsg [("turtlebot", turtlebotErrFlag, turtlebotDebugMsg)]
  -- Add your own values for debugging
  let specialDebugString = if errFlag then "DEBUG:: " ++ debugMsg else "STATE: simple :: "
  -- To stop simulation
  let debugString
        | errFlag = "STOPSIM " ++ specialDebugString
        | otherwise = specialDebugString

  -- Create OutputState
  let outputState = OutputState {outputData = outputData, errorFlag = errFlag, debugString = debugString}

  returnA -< outputState

analyzerSimpleState :: SF (String, OutputState) (Event (String))
analyzerSimpleState = proc (sfInput, sfOutput) -> do
  t <- time -< ()
  e <- edgeTag "endState" -< t > 5
  returnA -< e