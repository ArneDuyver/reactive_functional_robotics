{-# LANGUAGE Arrows #-}

module Controllogic where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.SimpleTwo
import Helpers.Controllers.OutputState

import Helpers.States.ErrorState

import Helpers.States.SimpleState
import Helpers.States.EndState

mapping :: SF String OutputState -> String -> SF String OutputState
mapping startingSF eventOutput
  | eventOutput == "simpleState"  = kSwitch simpleStateSF analyzerSimpleState mapping
  | eventOutput == "endState"  = kSwitch endStateSF analyzerEndState mapping
  | otherwise = errorStateSF


-- MAIN
mainSF :: SF String OutputState
mainSF = kSwitch simpleStateSF analyzerSimpleState mapping