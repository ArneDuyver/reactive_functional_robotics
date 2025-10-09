{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helpers.Controllers.Target where

import Data.Aeson (FromJSON(..), withObject, (.:), decode, Object, Result( Success ))
import Data.Aeson.Types (parseMaybe)
import Data.Bool (Bool (False, True))
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 qualified as B (pack)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (pack)

data TargetState = TargetState
  { xCoorTarget :: Double
  , yCoorTarget :: Double
  , width :: Double
  }
  deriving (Show, Generic)

instance FromJSON TargetState

-- Default Target state
defaultTargetState :: TargetState
defaultTargetState = TargetState
  { xCoorTarget = -1
  , yCoorTarget = -1
  , width = -1
  }

decodeTargetState :: String -> (TargetState, Bool, String)
decodeTargetState inputStr =
  case decode (B.pack inputStr) :: Maybe Object of
    Just obj ->
      case parseMaybe (.: "target") obj of
        Just targetObj ->
          (targetObj, False, "Successfully decoded TargetState")
        Nothing -> (defaultTargetState, True, "Key 'target' missing or invalid in input JSON")
    Nothing -> (defaultTargetState, True, "Input string is not valid JSON")

 