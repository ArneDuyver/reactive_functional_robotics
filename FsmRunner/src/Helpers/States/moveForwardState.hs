{-# LANGUAGE Arrows #-}

module Helpers.States.MoveForwardState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState

import Helpers.States.StateTemplate

-- Use the generic wrappers
moveForwardStateSF :: SF String OutputState
moveForwardStateSF = genericStateSF "MoveForward" stateBehaviour

analyzerMoveForwardState :: SF (String, OutputState) (Event String)
analyzerMoveForwardState = genericAnalyzerSF stateTransition


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
