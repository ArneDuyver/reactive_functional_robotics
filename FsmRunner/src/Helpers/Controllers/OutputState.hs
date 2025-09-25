{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helpers.Controllers.OutputState where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), decode, Object, Result( Success ))
import Data.Aeson.Types (parseMaybe)
import Data.Bool (Bool (False))
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 qualified as B (pack)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (pack)

data Turtlebot = Turtlebot
  { motorLeft :: Double
  , motorRight :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON Turtlebot

defaultTurtlebot :: Turtlebot
defaultTurtlebot = Turtlebot
  { motorLeft = 0.0
  , motorRight = 0.0
  }

data OutputData = OutputData
  { 
    turtlebot :: Turtlebot,
    state :: String
  }
  deriving (Show, Generic)

instance ToJSON OutputData

data OutputState = OutputState
  { outputData  :: OutputData
  , errorFlag   :: Bool
  , debugString :: String
  }
  deriving (Show, Generic)

instance ToJSON OutputState