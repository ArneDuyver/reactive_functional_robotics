{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helpers.Controllers.Turtlebot where

import Data.Aeson (FromJSON(..), withObject, (.:), decode, Object, Result( Success ))
import Data.Aeson.Types (parseMaybe)
import Data.Bool (Bool (False, True))
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 qualified as B (pack)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (pack)

data TurtlebotState = TurtlebotState
  { sensorFrontOne :: Double
  , sensorFrontTwo :: Double
  , sensorBackRight :: Double
  , sensorFrontRight :: Double
  , sensorBackLeft :: Double
  , sensorFrontLeft :: Double
  , sensorFrontOneDetect :: Integer
  , sensorFrontTwoDetect :: Integer
  , sensorBackRightDetect :: Integer
  , sensorFrontRightDetect :: Integer
  , sensorBackLeftDetect :: Integer
  , sensorFrontLeftDetect :: Integer
  , xCoor :: Double
  , yCoor :: Double
  , zRot :: Double
  }
  deriving (Show, Generic)

instance FromJSON TurtlebotState

-- Default Turtlebot state
defaultTurtlebotState :: TurtlebotState
defaultTurtlebotState = TurtlebotState
  { sensorFrontOne = -1
  , sensorFrontTwo = -1
  , sensorBackRight = -1
  , sensorFrontRight = -1
  , sensorBackLeft = -1
  , sensorFrontLeft = -1
  , sensorFrontOneDetect = -1
  , sensorFrontTwoDetect = -1
  , sensorBackRightDetect = -1
  , sensorFrontRightDetect = -1
  , sensorBackLeftDetect = -1
  , sensorFrontLeftDetect = -1
  , xCoor = -1
  , yCoor = -1
  , zRot = -1
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

 