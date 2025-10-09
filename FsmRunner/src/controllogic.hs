{-# LANGUAGE Arrows #-}

module Controllogic where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.OutputState

import Helpers.States.ErrorState

import Helpers.States.WallFollowState
import Helpers.States.EndState
import Helpers.States.MoveInOpeningState
import Helpers.States.TurnRightState
import Helpers.States.MoveOutOpeningState

mapping :: SF String OutputState -> String -> SF String OutputState
mapping startingSF eventOutput
  | eventOutput == "wallFollowState"  = kSwitch wallFollowStateSF analyzerWallFollowState mapping
  | eventOutput == "endState"  = kSwitch endStateSF analyzerEndState mapping
  | eventOutput == "moveInOpeningState"  = kSwitch moveInOpeningStateSF analyzerMoveInOpeningState mapping
  | eventOutput == "turnRightState"  = kSwitch turnRightStateSF analyzerTurnRightState mapping
  | eventOutput == "moveOutOpeningState"  = kSwitch moveOutOpeningStateSF analyzerMoveOutOpeningState mapping
  | otherwise = errorStateSF


-- MAIN
mainSF :: SF String OutputState
mainSF = kSwitch wallFollowStateSF analyzerWallFollowState mapping