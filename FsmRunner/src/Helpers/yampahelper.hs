{-# LANGUAGE Arrows #-}

module Helpers.YampaHelper where

import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Concurrent.STM (TBQueue, STM, atomically, readTBQueue, isEmptyTBQueue)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import FRP.Yampa (DTime, Event, SF, arr, constant, dSwitch, derivative, edge, hold, integral, reactimate, returnA, switch, tag, time)
-- import FRP.Yampa.Event
import Helpers.Simple
import Helpers.Turtlebot
-- import Helpers.KeyboardController
-- import Helpers.Controller

-- Debugging functions
createErrFlagAndDebugMsg :: [(String, Bool, String)] -> (Bool, String)
createErrFlagAndDebugMsg list = (errFlag, debugMsg)
  where errFlag = or [flag | (_, flag, _) <- list]
        debugMsg = unwords [str ++ ": " ++ msg | (str, flag, msg) <- list, flag]

-- Helper to drain TBQueue and get the latest message
getLatestMsg :: TBQueue a -> STM a
getLatestMsg queue = do
  x <- readTBQueue queue
  empty <- isEmptyTBQueue queue
  if empty
    then return x
    else getLatestMsg queue

-- Initialization
initialize :: TBQueue String -> MVar Integer -> IO (String)
initialize queue currentTimeVar = do
  msg <- atomically $ getLatestMsg queue
  currentTime <- getCurrentTimeInMs
  modifyMVar_ currentTimeVar $ \_ -> return currentTime
  return msg

-- Sense: Check for new value from MQTT (as an event)
sense :: TBQueue String -> MVar Integer -> Bool -> IO (DTime, Maybe (String))
sense queue currentTimeVar _ = do
  msg <- atomically $ getLatestMsg queue
  prevTime <- readMVar currentTimeVar
  currentTime <- getCurrentTimeInMs
  modifyMVar_ currentTimeVar $ \_ -> return currentTime
  let timeDiff :: DTime 
      timeDiff = fromIntegral (currentTime - prevTime) / 1000
  return (timeDiff, Just (msg))


-- DataTypes
-- data StateSetup = StateSetup
--   {
--     robot :: TurtlebotState,
--     keyboardController :: KeyboardControllerState,
--     errFlag :: Bool,
--     debugMsg :: String
--   } deriving (Show)

-- getStateSetupFromString :: String -> StateSetup
-- getStateSetupFromString inputStr =
--   let (turtlebot, turtlebotErrFlag, turtlebotDebugMsg) = decodeTurtlebotState inputStr
--       (keyboardcontroller, keyboardcontrollerErrFlag, keyboardcontrollerDebugMsg) = decodeKeyboardControllerState inputStr
--       (errFlag, debugMsg) = createErrFlagAndDebugMsg [("turtlebot", turtlebotErrFlag, turtlebotDebugMsg),("keyboardController", keyboardcontrollerErrFlag, keyboardcontrollerDebugMsg)]
--   in StateSetup
--       { robot = turtlebot
--       , keyboardController = keyboardcontroller
--       , errFlag = errFlag
--       , debugMsg = debugMsg
--       }

-- createDebugString :: StateSetup -> String -> String
-- createDebugString stateSetup infoString = 
--   let specialDebugString = if errFlag stateSetup then "DEBUG:: " ++ debugMsg stateSetup else infoString
--       debugString
--         | quit (keyboardController stateSetup) || errFlag stateSetup = "STOPSIM " ++ specialDebugString
--         | otherwise = specialDebugString
--   in debugString