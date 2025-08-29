{-# LANGUAGE ImportQualifiedPost #-}

module Helpers.MqttHelper where

import Control.Concurrent (readChan)
import Control.Concurrent.STM (TBQueue, atomically, writeTBQueue)
import Data.ByteString.Lazy.Char8 qualified as B (ByteString, pack, unpack)
import Data.Aeson (decode) 
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Network.MQTT.Client (MQTTClient, MessageCallback (SimpleCallback), connectURI, waitForClient, mqttConfig, publish, subscribe, _msgCB)
import Network.MQTT.Topic (Topic, mkFilter, mkTopic)
import Network.MQTT.Types (RetainHandling (DoNotSendOnSubscribe), Property, QoS, SubErr, subOptions)
import Network.URI (parseURI)
import Helpers.Turtlebot

subToTopic :: [Char] -> [Char] -> TBQueue String -> IO (([Either Network.MQTT.Types.SubErr QoS], [Property]), MQTTClient)
subToTopic brokerUri topic eventChan = do
  let (Just uri) = parseURI brokerUri
  mc <- connectURI mqttConfig {_msgCB = SimpleCallback (msgReceived eventChan)} uri
  let topicFilter = fromMaybe (error "Invalid topic filter") (mkFilter (pack topic))
  session <- subscribe mc [(topicFilter, subOptions)] []
  return (session, mc)

pubToTopic :: MQTTClient -> [Char] -> [Char] -> IO ()
pubToTopic mc topic msg = do -- mc instead of brokerUri
  let topicFilter = fromMaybe (error "Invalid topic filter") (mkTopic (pack topic))
  let msgInBytes = B.pack msg
  publish mc topicFilter msgInBytes False

msgReceived :: TBQueue String -> MQTTClient -> Topic -> B.ByteString -> [Property] -> IO ()
msgReceived eventChan _ topic msg payload = do
  atomically $ writeTBQueue eventChan (B.unpack msg)