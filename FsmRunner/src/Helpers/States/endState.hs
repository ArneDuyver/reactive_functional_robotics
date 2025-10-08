{-# LANGUAGE Arrows #-}

module Helpers.States.EndState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState


-- State behavior logic function - modify this to implement your state behavior
stateBehaviour :: SF TurtlebotState (Turtlebot, String)
stateBehaviour = proc turtlebot -> do
  -- Create default types for Output
  let turtlebotOut = defaultTurtlebot
      debugString = "STATE: end :: "  -- Customize this debug message
      -- Add your control logic here using the input parameters:
  returnA -< (turtlebotOut, debugString)

-- State transition logic function - determines next state based on inputs
stateTransition :: SF TurtlebotState (Bool, String)
stateTransition = proc turtlebot -> do
  -- Add your transition logic here using the input parameters:
  -- Return (shouldSwitch, targetStateName)
  t <- time -< ()
  let shouldSwitch = t > 5  -- Change this condition based on your own logic
      targetState = "newStateName"  -- Change this to the desired stateName
      -- Example logic based on sensor readings:
  returnA -< (shouldSwitch, targetState)






















endStateSF :: SF String OutputState
endStateSF = proc inputStr -> do
  -- Decode inputs for string
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState inputStr


  -- Use the stateBehaviour SF
  (turtlebotOut, stateDebugString) <- stateBehaviour -< turtlebot
 
  -- Create OutputData with state name
  let outputData = OutputData { turtlebot = turtlebotOut, state = "End" }
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

analyzerEndState :: SF (String, OutputState) (Event (String))
analyzerEndState = proc (sfInput, sfOutput) -> do
  -- Decode inputs for analysis
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState sfInput
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState sfInput


  -- Determine next state using transition logic
  (shouldSwitch, targetStateName) <- stateTransition -< turtlebot
  
  e <- edge -< shouldSwitch
  let eTagged = tag e targetStateName
  returnA -< eTagged