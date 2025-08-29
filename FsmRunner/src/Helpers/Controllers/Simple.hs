{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helpers.Controllers.Simple where

import Data.Aeson (FromJSON(..), withObject, (.:), decode, Object, Result( Success ))
import Data.Aeson.Types (parseMaybe)
import Data.Bool (Bool (False))
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 qualified as B (pack)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (pack)

data SimpleState = SimpleState
  { sensor :: Int
  }
  deriving (Show, Generic)

instance FromJSON SimpleState

-- Default Simple state
defaultSimpleState :: SimpleState
defaultSimpleState = SimpleState
  { sensor = 0  }

decodeSimpleState :: String -> (SimpleState, Bool, String)
decodeSimpleState inputStr =
  case decode (B.pack inputStr) :: Maybe Object of
    Just obj ->
      case parseMaybe (.: "simple") obj of
        Just simpleObj ->
          (simpleObj, False, "Successfully decoded SimpleState")
        Nothing -> (defaultSimpleState, True, "Key 'simple' missing or invalid in input JSON")
    Nothing -> (defaultSimpleState, True, "Input string is not valid JSON")
