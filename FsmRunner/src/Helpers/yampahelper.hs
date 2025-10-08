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

-- Pos vel is forward, pos rot is anti-clockwise turn
translationalAndRotationalVelocitiesToWheelVelocities :: Double -> Double -> Double -> Double -> (Double, Double)
translationalAndRotationalVelocitiesToWheelVelocities transVel rotVel wheelDistanceInM wheelRadiusInM =
  let -- Clamp input velocities to allowed ranges
      rotVelInverse = -rotVel
      clampedTrans = clamp (-maxTrans) maxTrans transVel
        where maxTrans = 1
      
      clampedRot = clamp (-maxRot) maxRot rotVelInverse
        where maxRot = 3.14 * 2
      
      -- Correct calculation using full wheel distance
      wheelDist = wheelDistanceInM
      leftVelRaw = (clampedTrans + (clampedRot * wheelDist / 2)) / wheelRadiusInM 
      rightVelRaw = (clampedTrans - (clampedRot * wheelDist / 2)) / wheelRadiusInM 
      
      -- Clamp wheel velocities to motor limits
      maxMotor = 3.14 * 20
      leftVel = clamp (-maxMotor) maxMotor leftVelRaw
      rightVel = clamp (-maxMotor) maxMotor rightVelRaw
  in (leftVel, rightVel)
  where
    clamp :: Double -> Double -> Double -> Double
    clamp lower upper = max lower . min upper

getTranslationalAndRotationalVelocities :: Double -> Double -> Double -> Double -> (Double, Double)
getTranslationalAndRotationalVelocities motorVelLeftRadSec motorVelRightRadSec wheelDistanceInM wheelRadiusInM = (vel, rot)
  where v_l = motorVelLeftRadSec * wheelRadiusInM
        v_r = motorVelRightRadSec * wheelRadiusInM
        vel = (v_r + v_l)/2
        rot = ((-1)*(v_r - v_l))/wheelDistanceInM

degreesToRadians :: Double -> Double
degreesToRadians deg = (deg / 180) * pi

radiansToDegrees :: Double -> Double
radiansToDegrees rad = (rad / pi) * 180