import System.IO (hSetBuffering, BufferMode(LineBuffering), stdout)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  mapM_ (\_ -> putStrLn "hello" >> threadDelay 1000000) [1..5]
