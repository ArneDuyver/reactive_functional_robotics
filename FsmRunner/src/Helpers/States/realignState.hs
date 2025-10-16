{-# LANGUAGE Arrows #-}

module Helpers.States.RealignState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState


-- State behavior logic function - modify this to implement your state behavior
stateBehaviour :: SF (TurtlebotState, TargetState) (Turtlebot, String)
stateBehaviour = proc (turtlebot, target) -> do
  -- Create default types for Output
  let turtlebotOut = defaultTurtlebot
      debugString = "STATE: realign :: "  -- Customize this debug message
      -- Add your control logic here using the input parameters
  returnA -< (turtlebotOut, debugString)

-- State transition logic function - determines next state based on inputs
stateTransition :: SF (TurtlebotState, TargetState) (Bool, String)
stateTransition = proc (turtlebot, target) -> do
  -- Add your transition logic here using the input parameters:
  -- Return (shouldSwitch, targetStateName)
  let shouldSwitch = False  -- Change this condition based on your logic
      targetState = "newStateName"  -- Target state name
      -- Add your transition logic here based on the input parameters
  returnA -< (shouldSwitch, targetState)






















realignStateSF :: SF String OutputState
realignStateSF = proc inputStr -> do
  -- Decode inputs for string
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState inputStr


  -- Use the stateBehaviour SF
  (turtlebotOut, stateDebugString) <- stateBehaviour -< (turtlebot, target)
 
  -- Create OutputData with state name
  let outputData = OutputData { turtlebot = turtlebotOut, state = "Realign" }
  -- Create the error string
  let (errFlag, debugMsg) = createErrFlagAndDebugMsg [ ("turtlebot", turtlebotErrFlag, turtlebotDebugMsg), ("target", targetErrFlag, targetDebugMsg) ]
  -- Add your own values for debugging
  let specialDebugString = if errFlag then "DEBUG:: " ++ debugMsg else stateDebugString
  -- To stop simulation
  let debugString
        | errFlag = "STOPSIM " ++ specialDebugString
        | otherwise = specialDebugString
  
  -- Create OutputState
  let outputState = OutputState { outputData = outputData, errorFlag = errFlag, debugString = debugString }

  returnA -< outputState

analyzerRealignState :: SF (String, OutputState) (Event (String))
analyzerRealignState = proc (sfInput, sfOutput) -> do
  -- Decode inputs for analysis
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState sfInput
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState sfInput


  -- Determine next state using transition logic
  (shouldSwitch, targetStateName) <- stateTransition -< (turtlebot, target)
  
  e <- edge -< shouldSwitch
  let eTagged = tag e targetStateName
  returnA -< eTagged