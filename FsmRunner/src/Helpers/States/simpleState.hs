{-# LANGUAGE Arrows #-}

module Helpers.States.SimpleState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState

-- State behavior logic function - modify this to implement your state behavior
stateBehaviour :: SF TurtlebotState (Turtlebot, String)
stateBehaviour = proc turtlebot -> do
  let turtlebotOut = Turtlebot {
                      motorLeft = fst (translationalAndRotationalVelocitiesToWheelVelocities  0.5 0.0 0.160 0.033),
                      motorRight = snd (translationalAndRotationalVelocitiesToWheelVelocities  0.5 0.0 0.160 0.033)
                    }
      debugString = "STATE: simple :: " ++ show (frontSensor turtlebot)
  returnA -< (turtlebotOut, debugString)

-- State transition logic function - determines next state based on inputs
stateTransition :: SF TurtlebotState (Bool, String)
stateTransition = proc turtlebot -> do
  t <- time -< ()
  let shouldSwitch = t > 2  
      targetState = "newStateName"  
  returnA -< (shouldSwitch, targetState)






















simpleStateSF :: SF String OutputState
simpleStateSF = proc inputStr -> do
  -- Decode inputs for string
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState inputStr


  -- Use the stateBehaviour SF
  (turtlebotOut, stateDebugString) <- stateBehaviour -< turtlebot
 
  -- Create OutputData with state name
  let outputData = OutputData { turtlebot = turtlebotOut, state = "Simple" }
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

analyzerSimpleState :: SF (String, OutputState) (Event (String))
analyzerSimpleState = proc (sfInput, sfOutput) -> do
  -- Decode inputs for analysis
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState sfInput
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState sfInput


  -- Determine next state using transition logic
  (shouldSwitch, targetStateName) <- stateTransition -< turtlebot
  
  e <- edge -< shouldSwitch
  let eTagged = tag e targetStateName
  returnA -< eTagged