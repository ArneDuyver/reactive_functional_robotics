{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helpers.Controllers.Controller where

import Data.Aeson (FromJSON(..), withObject, (.:), decode, Object, Result( Success ))
import Data.Aeson.Types (parseMaybe)
import Data.Bool (Bool (False, True))
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 qualified as B (pack)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (pack)

data ControllerState = ControllerState
  { yJoystick :: Double
  , xJoystick :: Double
  }
  deriving (Show, Generic)

instance FromJSON ControllerState

-- Default Controller state
defaultControllerState :: ControllerState
defaultControllerState = ControllerState
  { yJoystick = -1
  , xJoystick = -1
  }

decodeControllerState :: String -> (ControllerState, Bool, String)
decodeControllerState inputStr =
  case decode (B.pack inputStr) :: Maybe Object of
    Just obj ->
      case parseMaybe (.: "controller") obj of
        Just controllerObj ->
          (controllerObj, False, "Successfully decoded ControllerState")
        Nothing -> (defaultControllerState, True, "Key 'controller' missing or invalid in input JSON")
    Nothing -> (defaultControllerState, True, "Input string is not valid JSON")

 