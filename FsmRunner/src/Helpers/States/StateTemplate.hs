{-# LANGUAGE Arrows #-}

module Helpers.States.StateTemplate
  ( genericStateSF
  , genericAnalyzerSF
  ) where

import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.Target
import Helpers.Controllers.Controller
import Helpers.Controllers.OutputState


-- | Generic State Signal Function
-- Takes a state name and a stateBehaviour SF
genericStateSF
  :: String
  -> SF (TurtlebotState, TargetState, ControllerState) (Turtlebot, String)
  -> SF String OutputState
genericStateSF stateName stateBehaviour = proc inputStr -> do
  -- Decode inputs
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState inputStr
  let (controller, controllerErrFlag, controllerDebugMsg) = decodeControllerState inputStr


  -- Run the provided state behaviour
  (turtlebotOut, stateDebugString) <- stateBehaviour -< (turtlebot, target, controller)

  -- Create OutputData with the state name
  let outputData = OutputData { turtlebot = turtlebotOut, state = stateName }

  -- Handle errors and debugging
  let (errFlag, debugMsg) =
        createErrFlagAndDebugMsg
          [ ("turtlebot", turtlebotErrFlag, turtlebotDebugMsg), ("target", targetErrFlag, targetDebugMsg), ("controller", controllerErrFlag, controllerDebugMsg)
          ]
  let specialDebugString =
        if errFlag then "DEBUG:: " ++ debugMsg else stateDebugString
  let debugString =
        if errFlag then "STOPSIM " ++ specialDebugString else specialDebugString

  -- Produce OutputState
  let outputState =
        OutputState { outputData = outputData
                    , errorFlag = errFlag
                    , debugString = debugString
                    }

  returnA -< outputState


-- | Generic Analyzer Signal Function
-- Takes a stateTransition SF
genericAnalyzerSF
  :: SF (TurtlebotState, TargetState, ControllerState) (Bool, String)
  -> SF (String, OutputState) (Event String)
genericAnalyzerSF stateTransition = proc (sfInput, sfOutput) -> do
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState sfInput
  let (target, targetErrFlag, targetDebugMsg) = decodeTargetState sfInput
  let (controller, controllerErrFlag, controllerDebugMsg) = decodeControllerState sfInput


  (shouldSwitch, targetStateName) <- stateTransition -< (turtlebot, target, controller)
  e <- edge -< shouldSwitch
  let eTagged = tag e targetStateName
  returnA -< eTagged
