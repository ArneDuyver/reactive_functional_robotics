{-# LANGUAGE Arrows #-}

module Helpers.States.TurnLeftState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState

import Helpers.States.StateTemplate

-- Use the generic wrappers
turnLeftStateSF :: SF String OutputState
turnLeftStateSF = genericStateSF "TurnLeft" stateBehaviour

analyzerTurnLeftState :: SF (String, OutputState) (Event String)
analyzerTurnLeftState = genericAnalyzerSF stateTransition


stateBehaviour :: SF (TurtlebotState, TargetState) (Turtlebot, String)
stateBehaviour = proc (turtlebot, target) -> do
  let turtlebotOut = Turtlebot {
                      motorLeft = fst (translationalAndRotationalVelocitiesToWheelVelocities 0.0 0.2),
                      motorRight = snd (translationalAndRotationalVelocitiesToWheelVelocities 0.0 0.2)
                    }
      debugString = "STATE: turnLeft :: "  
  returnA -< (turtlebotOut, debugString)

stateTransition :: SF (TurtlebotState, TargetState) (Bool, String)
stateTransition = proc (turtlebot, target) -> do
  totalTurned <- integral -< 0.2
  
  let turnedDegrees = radiansToDegrees totalTurned
      targetDegrees = 90.0
      shouldSwitch = turnedDegrees >= targetDegrees
      targetState = "moveForwardState"
  returnA -< (shouldSwitch, targetState)
