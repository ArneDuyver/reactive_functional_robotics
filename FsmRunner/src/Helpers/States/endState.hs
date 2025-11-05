{-# LANGUAGE Arrows #-}

module Helpers.States.EndState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.Controller
import Helpers.Controllers.OutputState

import Helpers.States.StateTemplate

-- Use the generic wrappers
endStateSF :: SF String OutputState
endStateSF = genericStateSF "End" stateBehaviour

analyzerEndState :: SF (String, OutputState) (Event String)
analyzerEndState = genericAnalyzerSF stateTransition


-- Specific behaviour for End
stateBehaviour :: SF (TurtlebotState, TargetState, ControllerState) (Turtlebot, String)
stateBehaviour = proc (turtlebot, target, controller) -> do
  -- Create default types for Output
  let turtlebotOut = defaultTurtlebot
      debugString = "STATE: end :: "  -- Customize this debug message
      -- Add your control logic here using the input parameters
  returnA -< (turtlebotOut, debugString)

-- Specific transition for End
stateTransition :: SF (TurtlebotState, TargetState, ControllerState) (Bool, String)
stateTransition = proc (turtlebot, target, controller) -> do
  -- Add your transition logic here using the input parameters:
  -- Return (shouldSwitch, targetStateName)
  let shouldSwitch = False  -- Change this condition based on your logic
      targetState = "newStateName"  -- Target state name
      -- Add your transition logic here based on the input parameters
  returnA -< (shouldSwitch, targetState)