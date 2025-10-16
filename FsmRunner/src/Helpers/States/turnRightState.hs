{-# LANGUAGE Arrows #-}

module Helpers.States.TurnRightState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState


stateBehaviour :: SF (TurtlebotState, TargetState) (Turtlebot, String)
stateBehaviour = proc (turtlebot, target) -> do
  let turtlebotOut = Turtlebot {
                      motorLeft = fst (translationalAndRotationalVelocitiesToWheelVelocities 0.0 (-0.2)),
                      motorRight = snd (translationalAndRotationalVelocitiesToWheelVelocities 0.0 (-0.2))
                    }
      debugString = "STATE: turnRight :: " 
  returnA -< (turtlebotOut, debugString)

stateTransition :: SF (TurtlebotState, TargetState) (Bool, String)
stateTransition = proc (turtlebot, target) -> do
  let currentAngle = zRot turtlebot
  rec
    prevAngle <- iPre 0.0 -< currentAngle
    prevTotalTurn <- iPre 0.0 -< totalTurn
    let totalTurn = prevTotalTurn + (currentAngle - prevAngle)
    
  -- Normalize the angle difference to handle wraparound (e.g., -180 to 180)
  let normalizedDiff
        | totalTurn > pi = totalTurn - 2*pi
        | totalTurn < (-pi) = totalTurn + 2*pi
        | otherwise = totalTurn

      turnedDegrees = abs (radiansToDegrees normalizedDiff)
      targetDegrees = 90.0
      shouldSwitch = turnedDegrees >= targetDegrees
      targetState = "moveForwardState"
  returnA -< (shouldSwitch, targetState)






















turnRightStateSF :: SF String OutputState
turnRightStateSF = proc inputStr -> do
  -- Decode inputs for string
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState inputStr


  -- Use the stateBehaviour SF
  (turtlebotOut, stateDebugString) <- stateBehaviour -< (turtlebot, target)

  -- Create OutputData with state name
  let outputData = OutputData { turtlebot = turtlebotOut, state = "TurnRight" }
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

analyzerTurnRightState :: SF (String, OutputState) (Event (String))
analyzerTurnRightState = proc (sfInput, sfOutput) -> do
  -- Decode inputs for analysis
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState sfInput
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState sfInput


  -- Determine next state using transition logic
  (shouldSwitch, targetStateName) <- stateTransition -< (turtlebot, target)

  e <- edge -< shouldSwitch
  let eTagged = tag e targetStateName
  returnA -< eTagged