{-# LANGUAGE Arrows #-}

module Helpers.States.TurnRightState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState

import Helpers.States.StateTemplate

-- Use the generic wrappers
turnRightStateSF :: SF String OutputState
turnRightStateSF = genericStateSF "TurnRight" stateBehaviour

analyzerTurnRightState :: SF (String, OutputState) (Event String)
analyzerTurnRightState = genericAnalyzerSF stateTransition


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
