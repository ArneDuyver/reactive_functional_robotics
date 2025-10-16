{-# LANGUAGE Arrows #-}

module Controllogic where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState

import Helpers.States.ErrorState

import Helpers.States.MoveForwardState
import Helpers.States.EndState
import Helpers.States.TurnRightState
import Helpers.States.TurnLeftState
import Helpers.States.RealignState

mapping :: SF String OutputState -> String -> SF String OutputState
mapping startingSF eventOutput
  | eventOutput == "moveForwardState"  = kSwitch moveForwardStateSF analyzerMoveForwardState mapping
  | eventOutput == "endState"  = kSwitch endStateSF analyzerEndState mapping
  | eventOutput == "turnRightState"  = kSwitch turnRightStateSF analyzerTurnRightState mapping
  | eventOutput == "turnLeftState"  = kSwitch turnLeftStateSF analyzerTurnLeftState mapping
  | eventOutput == "realignState"  = kSwitch realignStateSF analyzerRealignState mapping
  | otherwise = errorStateSF


-- MAIN
mainSF :: SF String OutputState
mainSF = kSwitch moveForwardStateSF analyzerMoveForwardState mapping