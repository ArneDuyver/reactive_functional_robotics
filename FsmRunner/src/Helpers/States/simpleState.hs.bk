{-# LANGUAGE Arrows #-}

module Helpers.States.SimpleState where

import Control.Concurrent
import FRP.Yampa
import Helpers.YampaHelper
import Helpers.Controllers.Turtlebot
import Helpers.Controllers.OutputState




-- State behavior logic function - modify this to implement your state behavior
stateBehaviour :: TurtlebotState -> Turtlebot
stateBehaviour turtlebotState = defaultTurtlebot { motorLeft = 3.0, motorRight = 3.0 }
  -- Create default types for Output
      -- Add your control logic here using the input parameters:
            -- turtlebotOut' = turtlebotOut { 
      --   motorLeft = if (value turtlebot) > 0.5 then 1.0 else 0.0,
      --   motorRight = if (value turtlebot) > 0.5 then 1.0 else 0.0 
      -- }      -- 
      -- Access input sensor data like:
      -- - turtlebot sensor: (value turtlebot) gives you the sensor reading
      -- - turtlebot state: use existing parameter for feedback control

-- State transition logic function - determines next state based on inputs
stateTransition :: TurtlebotState -> (Bool, String)
stateTransition turtlebot = 
  -- Add your transition logic here using the input parameters:
  -- Return (shouldSwitch, targetStateName)
  let shouldSwitch = False  -- Change this condition based on your logic
      targetState = "endState"  -- Target state name
      -- Example logic based on sensor readings:
      -- shouldSwitch = (value turtlebot) > 0.8  -- Switch when sensor reading is high
      -- targetState = if (value turtlebot) > 0.8 then "FastState" 
      --              else if (value turtlebot) &lt; 0.2 then "SlowState"
      --              else "IdleState"
      --
      -- Access input data:
      -- - (value turtlebot): Get the sensor reading from turtlebot controller
      -- - turtlebot: Access previous turtlebot state for feedback-based decisions
  in (shouldSwitch, targetState)






















simpleStateSF :: SF String OutputState
simpleStateSF = proc inputStr -> do
  -- Decode inputs for string
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr


  let turtlebotOut = stateBehaviour turtlebot
 
  -- Create OutputData with state name
  let outputData = OutputData { turtlebot = turtlebotOut, state = "Simple" }
  -- Create the error string
  let (errFlag, debugMsg) = createErrFlagAndDebugMsg [ ("turtlebot", turtlebotErrFlag, turtlebotDebugMsg) ]
  -- Add your own values for debugging
  let specialDebugString = if errFlag then "DEBUG:: " ++ debugMsg else "STATE: simple :: "
  -- To stop simulation
  let debugString
        | errFlag = "STOPSIM " ++ specialDebugString
        | otherwise = specialDebugString
  
  -- Create OutputState
  let outputState = OutputState { outputData = outputData, errorFlag = errFlag, debugString = debugString }

  returnA -< outputState

analyzerSimpleState :: SF (String, OutputState) (Event (String))
analyzerSimpleState = proc (sfInput, sfOutput) -> do
  -- Decode inputs for analysis
  let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState sfInput


  -- Determine next state using transition logic
  let (shouldSwitch, targetState) = stateTransition turtlebot
  
  t <- time -< () 
  e <- edgeTag "endState" -< t > 4 
  returnA -< e