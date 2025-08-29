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

-- TODO automate also with multiple layers
data OutputData = OutputData
  { 
    out :: Int,
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