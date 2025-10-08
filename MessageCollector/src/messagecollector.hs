{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module MessageCollector where

import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, newMVar, putMVar, readChan, readMVar, takeMVar, threadDelay, killThread)
import Control.Concurrent.STM (STM, TBQueue, TVar, atomically, isEmptyTBQueue, lengthTBQueue, modifyTVar, newTBQueueIO, newTVarIO, readTBQueue, readTVar, readTVarIO, writeTBQueue, writeTVar)
import Control.Concurrent.STM.TBQueue (tryReadTBQueue)
import Control.Monad (when)
import Control.Exception (catch, SomeException, bracket)
import Data.Aeson (Object, Value, decode, encode)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as B (ByteString, pack, unpack)
import Data.HashMap.Strict qualified as HM
import Data.List
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Data.Yaml (FromJSON, ParseException, decodeFileEither)
import GHC.Generics (Generic)
import Network.MQTT.Client (MQTTClient, MessageCallback (SimpleCallback), connectURI, mqttConfig, normalDisconnect, publish, subscribe, waitForClient, _msgCB)
import Network.MQTT.Topic (Topic, mkFilter, mkTopic)
import Network.MQTT.Types (Property, QoS, RetainHandling (DoNotSendOnSubscribe), SubErr, subOptions)
import Network.URI (parseURI)
import System.Environment (getArgs)
import System.IO
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import System.Directory (doesFileExist, removeFile)

subToTopic :: [Char] -> [Char] -> TBQueue String -> IO (([Either Network.MQTT.Types.SubErr QoS], [Property]), MQTTClient)
subToTopic brokerUri topic eventChan = do
  let (Just uri) = parseURI brokerUri
  mc <- connectURI mqttConfig {_msgCB = SimpleCallback (msgReceived eventChan)} uri
  let topicFilter = fromMaybe (error "Invalid topic filter") (mkFilter (pack topic))
  session <- subscribe mc [(topicFilter, subOptions)] []
  return (session, mc)

pubToTopic :: MQTTClient -> [Char] -> [Char] -> IO ()
pubToTopic mc topic msg = do
  -- mc instead of brokerUri
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
    input_topics :: [String],
    required_input_topics :: [String]
  }
  deriving (Show, Generic)

data ConfigController = ConfigController
  { name :: String,
    critical :: Bool
  }
  deriving (Show, Generic)

data Configuration = Configuration
  { controllers :: [KM.KeyMap ConfigController]
  }
  deriving (Show, Generic)

instance FromJSON ConfigController

instance FromJSON Configuration

parseConfigurationFile :: String -> IO (Either String ([String], [String]))
parseConfigurationFile filepath = do
  result <- decodeFileEither filepath
  case result of
    Left err -> return $ Left $ "Failed to parse YAML: " ++ show err
    Right config -> do
      let allControllers = concatMap KM.elems (controllers config)
          allInputTopics = map ("fsm/input/" ++) (map name allControllers)
          requiredControllers = filter critical allControllers
          requiredInputTopics = map ("fsm/input/" ++) (map name requiredControllers)
      return $ Right (allInputTopics, requiredInputTopics)

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
      (input_topics, required_topics) = case configResult of
        Right (all_topics, req_topics) -> (all_topics, req_topics)
        Left err -> error $ "Failed to parse configuration: " ++ err
  return
    MqttConfig
      { host = broker,
        port = port,
        from_env_collection_topic = from_env_topic,
        to_env_collection_topic = to_env_topic,
        input_topics = input_topics,
        required_input_topics = required_topics
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

-- Check if shutdown was requested (simple file-based signal for Windows compatibility)
checkShutdownSignal :: IO Bool
checkShutdownSignal = do
  exists <- doesFileExist "messageCollector.stop"
  when exists $ do
    putStrLn "Shutdown signal received, cleaning up..."
    removeFile "messageCollector.stop"
  return exists

mqttCollector :: IO ()
mqttCollector = do
  -- Remove any existing stop signal file
  stopFileExists <- doesFileExist "messageCollector.stop"
  when stopFileExists $ removeFile "messageCollector.stop"
  
  mqttConfigVals <- createMqttConfigFromFile ".env"
  let brokerUriStr = host mqttConfigVals ++ ":" ++ show (port mqttConfigVals)
      maybeUri = parseURI brokerUriStr
      inputTopics = input_topics mqttConfigVals
      requiredTopics = required_input_topics mqttConfigVals
  print inputTopics -- TODO delete
  print ("Required topics: " ++ show requiredTopics) -- TODO delete
  case maybeUri of
    Just uri -> do
      stopSignal <- newEmptyMVar
      result <- catch (do
        mqttConnection <- connectURI mqttConfig uri
        chans <- startMqttListeners mqttConfigVals inputTopics stopSignal

        -- Track which required topics have sent at least one message
        receivedRequiredTopics <- newTVarIO Set.empty

        let loop lasts = do
              -- Check for shutdown signal
              shouldStop <- checkShutdownSignal
              if shouldStop
                then do
                  putStrLn "Graceful shutdown initiated"
                  return ()
                else do
                  msgs <- mapM (atomically . tryReadTBQueue) chans
                  let newLasts = zipWith (flip fromMaybe) msgs lasts
                      combinedMsg = combineJsons newLasts
                  pubToTopic mqttConnection (from_env_collection_topic mqttConfigVals) combinedMsg
                  if any (\msg -> "QUIT" `isPrefixOf` msg) newLasts
                    then return ()
                    else do
                      threadDelay 20000 -- Microseconds TODO: for testing use 1 second or so
                      loop newLasts

        let waitForRequiredDevices lasts = do
              -- Check for shutdown signal
              shouldStop <- checkShutdownSignal
              if shouldStop
                then do
                  putStrLn "Graceful shutdown during device wait"
                  return ()
                else do
                  msgs <- mapM (atomically . tryReadTBQueue) chans
                  let newLasts = zipWith (flip fromMaybe) msgs lasts
                      -- Check which topics have received messages (non-empty strings)
                      topicsWithMessages = [topic | (topic, msg) <- zip inputTopics newLasts, msg /= ""]
                      requiredTopicsWithMessages = filter (`elem` requiredTopics) topicsWithMessages

                  -- Update the set of received required topics
                  atomically $ do
                    received <- readTVar receivedRequiredTopics
                    let updatedReceived = foldr Set.insert received requiredTopicsWithMessages
                    writeTVar receivedRequiredTopics updatedReceived

                  -- Check if all required topics have sent at least one message
                  currentReceived <- readTVarIO receivedRequiredTopics
                  let allRequiredReceived = all (`Set.member` currentReceived) requiredTopics

                  if allRequiredReceived || null requiredTopics
                    then do
                      putStrLn "All required input devices have sent at least one message. Starting main loop..."
                      loop newLasts
                    else do
                      -- putStrLn $ "Waiting for required devices. Received: " ++ show (Set.toList currentReceived) ++ ", Required: " ++ show requiredTopics
                      threadDelay 100000 -- 100ms
                      waitForRequiredDevices newLasts

        waitForRequiredDevices (replicate (length chans) "")
        putStrLn "Disconnecting MQTT client..."
        normalDisconnect mqttConnection
        putMVar stopSignal ()
        putStrLn "MessageCollector stopped gracefully"
        return ())
        (\e -> do
          putStrLn $ "MessageCollector error: " ++ show (e :: SomeException)
          putMVar stopSignal ()
          return ())
      return ()
    Nothing -> error $ "Invalid broker URI: " ++ brokerUriStr
