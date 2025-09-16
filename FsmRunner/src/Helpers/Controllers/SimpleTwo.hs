{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helpers.Controllers.SimpleTwo where

import Data.Aeson (FromJSON(..), withObject, (.:), decode, Object, Result( Success ))
import Data.Aeson.Types (parseMaybe)
import Data.Bool (Bool (False, True))
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 qualified as B (pack)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (pack)

data SimpleTwoState = SimpleTwoState
  { testInput :: String
  }
  deriving (Show, Generic)

instance FromJSON SimpleTwoState

-- Default SimpleTwo state
defaultSimpleTwoState :: SimpleTwoState
defaultSimpleTwoState = SimpleTwoState
  { testInput = "Default String"
  }

decodeSimpleTwoState :: String -> (SimpleTwoState, Bool, String)
decodeSimpleTwoState inputStr =
  case decode (B.pack inputStr) :: Maybe Object of
    Just obj ->
      case parseMaybe (.: "simpleTwo") obj of
        Just simpleTwoObj ->
          (simpleTwoObj, False, "Successfully decoded SimpleTwoState")
        Nothing -> (defaultSimpleTwoState, True, "Key 'simpleTwo' missing or invalid in input JSON")
    Nothing -> (defaultSimpleTwoState, True, "Input string is not valid JSON")

 