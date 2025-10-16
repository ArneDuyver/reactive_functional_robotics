{-# LANGUAGE Arrows #-}

module Helpers.States.MoveForwardState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState


stateBehaviour :: SF (TurtlebotState, TargetState) (Turtlebot, String)
stateBehaviour = proc (turtlebot, target) -> do
  let turtlebotOut = Turtlebot {
                        motorLeft = fst (translationalAndRotationalVelocitiesToWheelVelocities 0.3 0.0),
                        motorRight = snd (translationalAndRotationalVelocitiesToWheelVelocities 0.3 0.0)
                    }
      debugString = "STATE: moveForward :: "
  returnA -< (turtlebotOut, debugString)

stateTransition :: SF (TurtlebotState, TargetState) (Bool, String)
stateTransition = proc (turtlebot, target) -> do
  let reachedTarget = distanceBetweenPoints (xCoor turtlebot, yCoor turtlebot) (xCoorTarget target, yCoorTarget target) < width target
  let reachedWall = sensorFrontOne turtlebot > 0 && sensorFrontOne turtlebot < 0.35
  let opening = if (sensorFrontRightDetect turtlebot == 0) then "right" 
                else if (sensorFrontLeftDetect turtlebot == 0) then "left"
                else "back"
  let shouldSwitch = reachedTarget || reachedWall
  let targetState
        | reachedTarget = "endState"
        | reachedWall = if opening == "right" then "turnRightState"
                        else if opening == "left" then "turnLeftState"
                        else "errorState"
        | otherwise = "errorState"

  returnA -< (shouldSwitch, targetState)






















moveForwardStateSF :: SF String OutputState
moveForwardStateSF = proc inputStr -> do
  -- Decode inputs for string
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState inputStr


  -- Use the stateBehaviour SF
  (turtlebotOut, stateDebugString) <- stateBehaviour -< (turtlebot, target)
 
  -- Create OutputData with state name
  let outputData = OutputData { turtlebot = turtlebotOut, state = "MoveForward" }
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

analyzerMoveForwardState :: SF (String, OutputState) (Event (String))
analyzerMoveForwardState = proc (sfInput, sfOutput) -> do
  -- Decode inputs for analysis
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState sfInput
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState sfInput


  -- Determine next state using transition logic
  (shouldSwitch, targetStateName) <- stateTransition -< (turtlebot, target)
  
  e <- edge -< shouldSwitch
  let eTagged = tag e targetStateName
  returnA -< eTagged