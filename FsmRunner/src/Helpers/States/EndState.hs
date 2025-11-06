{-# LANGUAGE Arrows #-}

module Helpers.States.EndState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState

import Helpers.States.StateTemplate

-- Use the generic wrappers
endStateSF :: SF String OutputState
endStateSF = genericStateSF "End" stateBehaviour

analyzerEndState :: SF (String, OutputState) (Event String)
analyzerEndState = genericAnalyzerSF stateTransition


stateBehaviour :: SF (TurtlebotState, TargetState) (Turtlebot, String)
stateBehaviour = proc (turtlebot, target) -> do
  let turtlebotOut = defaultTurtlebot
  t <- time -< ()
  let debugString 
        | t > 3 = "STOPSIM STATE: end :: "
        | otherwise = "STATE: end :: "
  returnA -< (turtlebotOut, debugString)

stateTransition :: SF (TurtlebotState, TargetState) (Bool, String)
stateTransition = proc (turtlebot, target) -> do
  let shouldSwitch = False  
      targetState = "newStateName" 
  returnA -< (shouldSwitch, targetState)
