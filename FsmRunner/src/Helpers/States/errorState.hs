{-# LANGUAGE Arrows #-}

module Helpers.States.ErrorState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.SimpleTwo
import Helpers.Controllers.OutputState



errorStateSF :: SF String OutputState
errorStateSF = proc inputStr -> do
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr

  -- Create default types for Output using default constructors
  let turtlebotOut = defaultTurtlebot


  -- Create OutputData with error state name
  let outputData = OutputData { turtlebot = turtlebotOut, state = "Error" }
  -- Create the error string for unknown state transitions
  let errFlag = True
  let debugMsg = "ERROR: Unknown state transition occurred"
  let specialDebugString = "DEBUG:: " ++ debugMsg
  -- To stop simulation
  let debugString = "STOPSIM " ++ specialDebugString
  
  -- Create OutputState with error flag set to true
  let outputState = OutputState { outputData = outputData, errorFlag = errFlag, debugString = debugString }

  returnA -< outputState

analyzerErrorState :: SF (String, OutputState) (Event (String))
analyzerErrorState = proc (sfInput, sfOutput) -> do
  -- Error state never transitions - it's a terminal state
  returnA -< NoEvent
