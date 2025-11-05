{-# LANGUAGE Arrows #-}

module Controllogic where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.Controller
import Helpers.Controllers.OutputState

import Helpers.States.ErrorState

import Helpers.States.ControllerState
import Helpers.States.EndState

mapping :: SF String OutputState -> String -> SF String OutputState
mapping startingSF eventOutput
  | eventOutput == "controllerState"  = kSwitch controllerStateSF analyzerControllerState mapping
  | eventOutput == "endState"  = kSwitch endStateSF analyzerEndState mapping
  | otherwise = errorStateSF


-- MAIN
mainSF :: SF String OutputState
mainSF = kSwitch controllerStateSF analyzerControllerState mapping