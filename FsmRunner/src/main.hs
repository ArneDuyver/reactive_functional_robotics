{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Concurrent (forkIO, newMVar, putMVar, takeMVar, newEmptyMVar, readChan, readMVar, threadDelay)
import Control.Concurrent.STM (TBQueue, STM, newTBQueueIO, atomically, writeTBQueue, readTBQueue, lengthTBQueue, isEmptyTBQueue)
import FRP.Yampa (SF, derivative, reactimate, returnA, time)
import Network.MQTT.Client (MQTTClient, connectURI, mqttConfig, waitForClient, normalDisconnect)
import Network.URI (parseURI)
import Data.Char
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as B (pack, unpack)
-- import FRP.Yampa.Event
import Helpers.MqttHelper
import Helpers.Simple
import Helpers.YampaHelper
import Helpers.Turtlebot
import Controllogic
import Control.Monad (unless, when)
import Helpers.Controllers.OutputState
-- FIXME TODO get from .env file
mqttBroker :: String
mqttBroker = "mqtt://localhost:1883"

fromSimTopic :: String
fromSimTopic = "test/fromsim"

toSimTopic :: String
toSimTopic = "test/tosim"

main :: IO ()
main = do
  eventChan <- newTBQueueIO 10 -- Create event queue for messages received by MQTT, capacity 10
  stopSignal <- newEmptyMVar
  forkIO $ do
    -- Create a parallel subprocess to listen for MQTT messages
    (session, mc) <- subToTopic mqttBroker fromSimTopic eventChan
    waitForClient mc
    takeMVar stopSignal  -- Wait for signal to stop
    normalDisconnect mc
  currentTime <- getCurrentTimeInMs -- Save the current time in Ms to compare new messages to
  currentTimeGlobal <- newMVar currentTime
  let initialValue = initialize eventChan currentTimeGlobal
  let inputStream = sense eventChan currentTimeGlobal
  let (Just uri) = parseURI mqttBroker
  mqttConnection <- connectURI mqttConfig uri
  _ <- reactimate initialValue inputStream (actuate mqttConnection) mainSF
  normalDisconnect mqttConnection
  putMVar stopSignal ()
  
-- Actuate: do what needs to happen with the output of the reactive network
actuate :: MQTTClient -> Bool -> OutputState -> IO Bool
actuate mqttConnection _ value = do
  let fsmoutput = outputData value
  let errFlag = errorFlag value
  let debugMsg = debugString value
  -- print value
  pubToTopic mqttConnection toSimTopic (B.unpack (encode value))
  let stopSim = take 7 (debugMsg) == "STOPSIM"
  return stopSim -- False means the program keeps running

-- FOR DEBUGGING


