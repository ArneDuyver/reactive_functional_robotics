module Helpers.Simple where

import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Text.Read (readMaybe)

getCurrentTimeInMs :: IO Integer
getCurrentTimeInMs = do
  currentTime <- getCurrentTime
  let posixTime = utcTimeToPOSIXSeconds currentTime
  return $ round (posixTime * 1000)

roundedTime t = (fromIntegral (round (t * 10000)) :: Double) / 10000

-- TESTING TODO change to parse turtlebot JSON
parseInput :: String -> Maybe (Double, Double)
parseInput input = case splitOn ';' input of
  [sideStr, frontStr] -> do
    side <- readMaybe sideStr
    front <- readMaybe frontStr
    return (side, front)
  _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr (\c acc -> if c == delimiter then [] : acc else (c : head acc) : tail acc) [[]]
