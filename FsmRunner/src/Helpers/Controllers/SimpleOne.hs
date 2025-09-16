{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helpers.Controllers.SimpleOne where

import Data.Aeson (FromJSON(..), withObject, (.:), decode, Object, Result( Success ))
import Data.Aeson.Types (parseMaybe)
import Data.Bool (Bool (False, True))
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 qualified as B (pack)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (pack)

data SimpleOneState = SimpleOneState
  { sensorOne :: Integer
  }
  deriving (Show, Generic)

instance FromJSON SimpleOneState

-- Default SimpleOne state
defaultSimpleOneState :: SimpleOneState
defaultSimpleOneState = SimpleOneState
  { sensorOne = -9999
  }

decodeSimpleOneState :: String -> (SimpleOneState, Bool, String)
decodeSimpleOneState inputStr =
  case decode (B.pack inputStr) :: Maybe Object of
    Just obj ->
      case parseMaybe (.: "simpleOne") obj of
        Just simpleOneObj ->
          (simpleOneObj, False, "Successfully decoded SimpleOneState")
        Nothing -> (defaultSimpleOneState, True, "Key 'simpleOne' missing or invalid in input JSON")
    Nothing -> (defaultSimpleOneState, True, "Input string is not valid JSON")

 