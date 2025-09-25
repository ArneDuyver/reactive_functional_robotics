{-# LANGUAGE Arrows #-}

module Helpers.States.EndState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.SimpleTwo
import Helpers.Controllers.OutputState


endStateSF :: SF String OutputState
endStateSF = proc inputStr -> do
  -- Decode inputs for string
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr

  -- Create default types for Output
  let turtlebotOut = Turtlebot { motorLeft = 0, motorRight = 0 }

    
  -- #### Control logic CHANGE THE DEFAULT OUTPUT VALUES TO THE DESIRED VALUE ####

  -- #### END: Control logic ####

  -- Create OutputData with state name
  let outputData = OutputData { turtlebot = turtlebotOut, state = "End" }
  -- Create the error string
  let (errFlag, debugMsg) = createErrFlagAndDebugMsg [ ("turtlebot", turtlebotErrFlag, turtlebotDebugMsg) ]
  -- Add your own values for debugging
  let specialDebugString = if errFlag then "DEBUG:: " ++ debugMsg else "STATE: end :: "
  -- To stop simulation
  let debugString
        | errFlag = "STOPSIM " ++ specialDebugString
        | otherwise = specialDebugString
  
  -- Create OutputState
  let outputState = OutputState { outputData = outputData, errorFlag = errFlag, debugString = debugString }

  returnA -< outputState

analyzerEndState :: SF (String, OutputState) (Event (String))
analyzerEndState = proc (sfInput, sfOutput) -> do
  e <- edgeTag "newStateName" -< True 
  returnA -< e