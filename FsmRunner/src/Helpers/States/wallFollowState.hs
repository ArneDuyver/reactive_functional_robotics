{-# LANGUAGE Arrows #-}

module Helpers.States.WallFollowState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState

-- State behavior logic function - modify this to implement your state behavior
stateBehaviour :: SF (TurtlebotState, TargetState) (Turtlebot, String)
stateBehaviour = proc (turtlebot, target) -> do
  let turtlebotOut = Turtlebot {
                      motorLeft = fst (translationalAndRotationalVelocitiesToWheelVelocities  0.4 0.0),
                      motorRight = snd (translationalAndRotationalVelocitiesToWheelVelocities  0.4 0.0)
                    }
  let distanceToTarget = distanceBetweenPoints ((xCoor turtlebot), (yCoor turtlebot)) ((xCoorTarget target), (yCoorTarget target))
  let shouldSwitch = distanceToTarget < (width target)
      debugString = "STATE: wallFollow :: "  ++ show distanceToTarget ++ " | " ++ show (width target) ++ "|" ++ show ((xCoor turtlebot), (yCoor turtlebot)) ++ "|" ++ show ((xCoorTarget target), (yCoorTarget target))
  returnA -< (turtlebotOut, debugString)

-- State transition logic function - determines next state based on inputs
stateTransition :: SF (TurtlebotState, TargetState) (Bool, String)
stateTransition = proc (turtlebot, target) -> do
  let distanceToTarget = distanceBetweenPoints ((xCoor turtlebot), (yCoor turtlebot)) ((xCoorTarget target), (yCoorTarget target))
  let shouldSwitch = distanceToTarget < (width target)
      targetState = "endState"
  returnA -< (shouldSwitch, targetState)






















wallFollowStateSF :: SF String OutputState
wallFollowStateSF = proc inputStr -> do
  -- Decode inputs for string
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState inputStr


  -- Use the stateBehaviour SF
  (turtlebotOut, stateDebugString) <- stateBehaviour -< (turtlebot, target)
 
  -- Create OutputData with state name
  let outputData = OutputData { turtlebot = turtlebotOut, state = "WallFollow" }
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

analyzerWallFollowState :: SF (String, OutputState) (Event (String))
analyzerWallFollowState = proc (sfInput, sfOutput) -> do
  -- Decode inputs for analysis
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState sfInput
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState sfInput


  -- Determine next state using transition logic
  (shouldSwitch, targetStateName) <- stateTransition -< (turtlebot, target)
  
  e <- edge -< shouldSwitch
  let eTagged = tag e targetStateName
  returnA -< eTagged