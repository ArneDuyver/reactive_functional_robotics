{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helpers.Turtlebot where

import Data.Aeson (FromJSON(..), withObject, (.:), decode, Object, Result( Success ))
import Data.Aeson.Types (parseMaybe)
import Data.Bool (Bool (False))
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 qualified as B (pack)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (pack)

data TurtlebotState = TurtlebotState
  { sensorValues :: [Double],
    imuAcceleration:: [Double],
    imuGyro:: [Double],
    sensorDefault :: Double,
    lidarSensorMaxInM :: Double,
    motorVelMaxInRadSec :: Double,
    motorVelLeftRadSec :: Double,
    motorVelRightRadSec :: Double,
    maxTranslationalSpeedInMSec :: Double,
    translationalSpeedInMSec :: Double,
    maxRotationalSpeedInRadSec :: Double,
    rotationalSpeedInRadSec :: Double,
    wheelDistanceInM :: Double,
    wheelRadiusInM :: Double,
    position :: [Double],
    orientation :: [Double],
    otherSensorValues :: [Double]
  }
  deriving (Show, Generic)

instance FromJSON TurtlebotState 


defaultTurtlebotState :: TurtlebotState
defaultTurtlebotState = TurtlebotState 
  { 
    sensorValues = [],
    imuAcceleration = [],
    imuGyro = [],
    sensorDefault = -9999,
    lidarSensorMaxInM = -9999,
    motorVelMaxInRadSec = -9999,
    motorVelLeftRadSec = -9999,
    motorVelRightRadSec = -9999,
    maxTranslationalSpeedInMSec = -9999,
    translationalSpeedInMSec = -9999,
    maxRotationalSpeedInRadSec = -9999,
    rotationalSpeedInRadSec = -9999,
    wheelDistanceInM = -9999,
    wheelRadiusInM = -9999,
    position = [],
    orientation = [],
    otherSensorValues = []
  }

decodeTurtlebotState :: String -> (TurtlebotState, Bool, String)
decodeTurtlebotState inputStr =
  case decode (B.pack inputStr) :: Maybe Object of
    Just obj ->
      case parseMaybe (.: "turtlebot") obj of
        Just turtlebotObj ->
          (turtlebotObj, False, "Successfully decoded TurtlebotState")
        Nothing -> (defaultTurtlebotState, True, "Key 'turtlebot' missing or invalid in input JSON")
    Nothing -> (defaultTurtlebotState, True, "Input string is not valid JSON")


-- EXTRA FUNCTIONS 
-- Pos vel is forward, pos rot is anti-clockwise turn
translationalAndRotationalVelocitiesToWheelVelocities :: Double -> Double -> TurtlebotState -> (Double, Double)
translationalAndRotationalVelocitiesToWheelVelocities transVel rotVelInverse turtlebot =
  let -- Clamp input velocities to allowed ranges
      rotVel = -rotVelInverse
      clampedTrans = clamp (-maxTrans) maxTrans transVel
        where maxTrans = maxTranslationalSpeedInMSec turtlebot
      
      clampedRot = clamp (-maxRot) maxRot rotVel
        where maxRot = maxRotationalSpeedInRadSec turtlebot
      
      -- Correct calculation using full wheel distance
      wheelDist = wheelDistanceInM turtlebot
      leftVelRaw = (clampedTrans + (clampedRot * wheelDist / 2)) / wheelRadiusInM turtlebot
      rightVelRaw = (clampedTrans - (clampedRot * wheelDist / 2)) / wheelRadiusInM turtlebot
      
      -- Clamp wheel velocities to motor limits
      maxMotor = motorVelMaxInRadSec turtlebot
      leftVel = clamp (-maxMotor) maxMotor leftVelRaw
      rightVel = clamp (-maxMotor) maxMotor rightVelRaw
  in (leftVel, rightVel)
  where
    clamp :: Double -> Double -> Double -> Double
    clamp lower upper = max lower . min upper

getTranslationalAndRotationalVelocities :: TurtlebotState -> (Double, Double)
getTranslationalAndRotationalVelocities turtlebot = (vel, rot)
  where v_l = motorVelLeftRadSec turtlebot * wheelRadiusInM turtlebot
        v_r = motorVelRightRadSec turtlebot * wheelRadiusInM turtlebot
        vel = (v_r + v_l)/2
        rot = ((-1)*(v_r - v_l))/wheelDistanceInM turtlebot

degreesToRadians :: Double -> Double
degreesToRadians deg = (deg / 180) * pi

radiansToDegrees :: Double -> Double
radiansToDegrees rad = (rad / pi) * 180

