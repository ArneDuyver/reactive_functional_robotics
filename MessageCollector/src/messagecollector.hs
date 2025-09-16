{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module MessageCollector where

import System.IO
import Data.List
import Data.Maybe
import GHC.Generics (Generic)
import System.Environment (getArgs)
import Control.Concurrent (ThreadId, MVar, forkIO, newMVar, putMVar, takeMVar, newEmptyMVar, readChan, readMVar, threadDelay)
import Control.Concurrent.STM (TBQueue, STM, newTBQueueIO, atomically, writeTBQueue, readTBQueue, lengthTBQueue, isEmptyTBQueue)
import Control.Concurrent.STM.TBQueue (tryReadTBQueue)
import Data.ByteString.Lazy.Char8 qualified as B (ByteString, pack, unpack)
import Data.Aeson (decode, encode, Object, Value)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.HashMap.Strict as HM
import Network.MQTT.Client (MQTTClient, MessageCallback (SimpleCallback),normalDisconnect, connectURI, waitForClient, mqttConfig, publish, subscribe, _msgCB)
import Network.MQTT.Topic (Topic, mkFilter, mkTopic)
import Network.MQTT.Types (RetainHandling (DoNotSendOnSubscribe), Property, QoS, SubErr, subOptions)
import Network.URI (parseURI)
import Control.Monad (when)
import Data.Text (pack, unpack)
import Data.Yaml (decodeFileEither, ParseException, FromJSON)

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

getLatestMsg :: TBQueue a -> STM a
getLatestMsg queue = do
  x <- readTBQueue queue
  empty <- isEmptyTBQueue queue
  if empty
    then return x
    else getLatestMsg queue

getEnvVarValue :: String -> IO (Maybe String)
getEnvVarValue varName = do
    let envFile = "config/.env"
    contents <- readFile envFile
    return $ lookupEnvVar varName contents

lookupEnvVar :: String -> String -> Maybe String
lookupEnvVar name contents =
    let ls = lines contents
        match = find (\l -> (name ++ "=") `isPrefixOf` l) ls
    in fmap (drop (length name + 1)) match


data MqttConfig = MqttConfig
  { host :: String,
    port :: Integer,
    from_env_collection_topic :: String,
    to_env_collection_topic :: String,
    input_topics :: [String]
  }
  deriving (Show, Generic)

data ConfigController = ConfigController
  { name :: String
  }
  deriving (Show, Generic)

data Configuration = Configuration
  { controllers :: [KM.KeyMap ConfigController]
  }
  deriving (Show, Generic)

instance FromJSON ConfigController
instance FromJSON Configuration

parseConfigurationFile :: String -> IO (Either String [String])
parseConfigurationFile filepath = do
  result <- decodeFileEither filepath
  case result of
    Left err -> return $ Left $ "Failed to parse YAML: " ++ show err
    Right config -> do
      let controllerNames = concatMap (map name . KM.elems) (controllers config)
          inputTopics = map ("fsm/input/" ++) controllerNames
      return $ Right inputTopics
  
createMqttConfigFromFile :: String -> IO MqttConfig
createMqttConfigFromFile filepath = do
  brokerVal <- getEnvVarValue "MQTT_BROKER_HOST"
  portVal <- getEnvVarValue "MQTT_BROKER_PORT"
  from_env_topicVal <- getEnvVarValue "FROM_ENV_COLLECTION_TOPIC"
  to_env_topicVal <- getEnvVarValue "TO_ENV_COLLECTION_TOPIC"
  configResult <- parseConfigurationFile "config/configuration.yml"
  let broker = fromMaybe "NOT FOUND" brokerVal
      port = maybe (-9999) readInt portVal
      from_env_topic = fromMaybe "NOT FOUND" from_env_topicVal
      to_env_topic = fromMaybe "NOT FOUND" to_env_topicVal
      input_topics = case configResult of
        Right topics -> topics
        Left err -> error $ "Failed to parse configuration: " ++ err
  return MqttConfig {
    host = broker,
    port = port,
    from_env_collection_topic = from_env_topic,
    to_env_collection_topic = to_env_topic,
    input_topics = input_topics
  }
  where
    readInt :: String -> Integer
    readInt s = case reads s of
      [(i, "")] -> i
      _ -> -9999

startMqttListener :: MqttConfig -> String -> MVar () -> IO (TBQueue String, ThreadId)
startMqttListener mqttConfig topic stopSignal = do
  eventChan <- newTBQueueIO 10
  tid <- forkIO $ do
    let brokerUri = host mqttConfig ++ ":" ++ show (port mqttConfig)
    (session, mc) <- subToTopic brokerUri topic eventChan
    waitForClient mc
    takeMVar stopSignal
    normalDisconnect mc
  return (eventChan, tid)

startMqttListeners :: MqttConfig -> [String] -> MVar () -> IO [TBQueue String]
startMqttListeners mqttConfig topics stopSignal = do
  chansAndThreads <- mapM (\topic -> startMqttListener mqttConfig topic stopSignal) topics
  let chans = map fst chansAndThreads
  return chans

combineJsons :: [String] -> String
combineJsons msgs =
  let objects = mapMaybe (decode . B.pack) msgs :: [KM.KeyMap Data.Aeson.Value]
      merged = foldr KM.union KM.empty objects
  in B.unpack (encode merged)

mqttCollector :: IO ()
mqttCollector = do
  mqttConfigVals <- createMqttConfigFromFile ".env"
  let brokerUriStr = host mqttConfigVals ++ ":" ++ show (port mqttConfigVals)
      maybeUri = parseURI brokerUriStr
      inputTopics = input_topics mqttConfigVals
  print inputTopics --TODO delete
  case maybeUri of
    Just uri -> do
      stopSignal <- newEmptyMVar
      mqttConnection <- connectURI mqttConfig uri
      chans <- startMqttListeners mqttConfigVals inputTopics stopSignal
      let loop lasts = do
            msgs <- mapM (atomically . tryReadTBQueue) chans
            let newLasts = zipWith (flip fromMaybe) msgs lasts
                combinedMsg = combineJsons newLasts
            pubToTopic mqttConnection (to_env_collection_topic mqttConfigVals) combinedMsg
            if any (\msg -> "QUIT" `isPrefixOf` msg) newLasts
              then return ()
              else do
                threadDelay 20000 --Microseconds TODO: for testing use 1 second or so
                loop newLasts
      loop (replicate (length chans) "")
      normalDisconnect mqttConnection
      putMVar stopSignal ()
    Nothing -> error $ "Invalid broker URI: " ++ brokerUriStr

