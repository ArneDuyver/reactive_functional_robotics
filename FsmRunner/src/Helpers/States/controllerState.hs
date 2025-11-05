{-# LANGUAGE Arrows #-}

module Helpers.States.ControllerState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.Controller
import Helpers.Controllers.OutputState

import Helpers.States.StateTemplate

-- Use the generic wrappers
controllerStateSF :: SF String OutputState
controllerStateSF = genericStateSF "Controller" stateBehaviour

analyzerControllerState :: SF (String, OutputState) (Event String)
analyzerControllerState = genericAnalyzerSF stateTransition


-- Specific behaviour for Controller
stateBehaviour :: SF (TurtlebotState, TargetState, ControllerState) (Turtlebot, String)
stateBehaviour = proc (turtlebot, target, controller) -> do
  let joystickMax = 32768
  let deadzone = 400
  let maxTransSpeed = 1.5
  let maxRotSpeed = 1.5
  let transSpeed = mapJoystick joystickMax (-joystickMax) deadzone maxTransSpeed (-maxTransSpeed) (yJoystick controller)
  let rotSpeed = mapJoystick joystickMax (-joystickMax) deadzone maxRotSpeed (-maxRotSpeed) (-(xJoystick controller))
  let turtlebotOut = Turtlebot {
                        motorLeft = fst (translationalAndRotationalVelocitiesToWheelVelocities transSpeed rotSpeed),
                        motorRight = snd (translationalAndRotationalVelocitiesToWheelVelocities transSpeed rotSpeed)
                    }
      debugString = "STATE: controller :: "  ++ show transSpeed ++ " | " ++ show rotSpeed
  returnA -< (turtlebotOut, debugString)


-- Specific transition for Controller
stateTransition :: SF (TurtlebotState, TargetState, ControllerState) (Bool, String)
stateTransition = proc (turtlebot, target, controller) -> do
  let shouldSwitch = False 
      targetState = "newStateName"  
  returnA -< (shouldSwitch, targetState)

mapJoystick :: Double -> Double -> Double -> Double -> Double -> Double -> Double
mapJoystick max min deadzone exitmax exitmin value 
  | abs value <= deadzone = 0
  | value > deadzone = ((value - deadzone) / (max - deadzone)) * exitmax
  | value < (-deadzone) = ((value + deadzone) / (min + deadzone)) * exitmin
  | otherwise = 0