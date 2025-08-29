{-# LANGUAGE Arrows #-}

module Controllogic where

import Control.Concurrent
import FRP.Yampa
import Helpers.Turtlebot
import Helpers.KeyboardController
import Helpers.YampaHelper
import Data.Bool (Bool (False))
import Numeric (showFFloat)

import Helpers.States.WallFollowState
import Helpers.States.EnterOpeningState
import Helpers.States.TurnRightState
import Helpers.States.TurnLeftState
import Helpers.States.ExitOpeningState
import Helpers.States.EndState

mapping :: SF String (Double, Double, Bool, String) -> String -> SF String (Double, Double, Bool, String)
mapping startingSF eventOutput
  | eventOutput == "wallFollowState"  = kSwitch wallFollowStateSF analyzerWallFollowState mapping
  | eventOutput == "enterOpeningState"  = kSwitch enterOpeningStateSF analyzerEnterOpeningState mapping
  | eventOutput == "turnRightState"  = kSwitch turnRightStateSF analyzerTurnRightState mapping
  | eventOutput == "turnLeftState"  = kSwitch turnLeftStateSF analyzerTurnLeftState mapping
  | eventOutput == "exitOpeningState"  = kSwitch exitOpeningStateSF analyzerExitOpeningState mapping
  | eventOutput == "endState"  = kSwitch endStateSF analyzerEndState mapping
  | otherwise = errorStateSF

-- MAIN
mainSF :: SF String (Double, Double, Bool, String)
mainSF = kSwitch wallFollowSF analyzerWallFollowState mapping
