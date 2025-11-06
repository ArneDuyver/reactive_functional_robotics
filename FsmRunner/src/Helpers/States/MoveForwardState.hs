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


-- Specific behaviour for MoveForward
stateBehaviour :: SF (TurtlebotState, TargetState) (Turtlebot, String)
stateBehaviour = proc (turtlebot, target) -> do
  -- Create default types for Output
  let turtlebotOut = defaultTurtlebot
      debugString = "STATE: moveForward :: "  -- Customize this debug message
      -- Add your control logic here using the input parameters
  returnA -< (turtlebotOut, debugString)

-- Specific transition for MoveForward
stateTransition :: SF (TurtlebotState, TargetState) (Bool, String)
stateTransition = proc (turtlebot, target) -> do
  -- Add your transition logic here using the input parameters:
  -- Return (shouldSwitch, targetStateName)
  let shouldSwitch = False  -- Change this condition based on your logic
      targetState = "newStateName"  -- Target state name
      -- Add your transition logic here based on the input parameters
  returnA -< (shouldSwitch, targetState)