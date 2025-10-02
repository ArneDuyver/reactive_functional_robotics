module FsmFrontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Process (std_out, readProcess, readCreateProcess, shell, createProcess, StdStream(CreatePipe), waitForProcess, terminateProcess, ProcessHandle)
import System.IO (hSetBuffering, BufferMode(LineBuffering), hIsEOF, hGetLine, Handle, hClose)
import Control.Monad (void)
import Data.IORef
import System.Exit (exitSuccess)
import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay)


-- outputRef: Stores accumulated output lines from running processes
-- PhRef: Reference to process handle for termination
-- ThreadRef: Reference to the thread reading output
-- handleRef: Reference to the stdout handle
-- window: The Threepenny-UI window object for the web interface
setup :: IORef [String] -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> Window -> UI ()
setup outputRef runnerPhRef runnerThreadRef runnerhandleRef mcPhRef mcThreadRef mcHandleRef window = do
    return window # set title "Reactive Functional Robotics"

    -- Load the homepage.html file and set it as the body content
    htmlContent <- liftIO $ readFile "./FsmFrontend/static/homepage.html"
    getBody window # set html htmlContent
    
    -- Wait a moment for DOM to be ready
    liftIO $ threadDelay 500000 -- 500ms to ensure DOM is loaded

    -- Create Threepenny buttons with proper CSS classes and IDs
    runButton <- UI.button # set text "Run" 
                          # set (attr "class") "btn btn-run"
    buildButton <- UI.button # set text "Build"
                            # set (attr "class") "btn btn-build"
    clearButton <- UI.button # set text "Clear"
                            # set (attr "class") "btn btn-clear"
    quitButton <- UI.button # set text "Quit"
                           # set (attr "class") "btn btn-quit"

    -- Create Threepenny display area
    displayArea <- UI.div # set (attr "class") "display-area"
                          # set text "Press a button to start ..."

    -- Add buttons to the button container
    buttonContainer <- getElementById window "threepenny-buttons"
    case buttonContainer of
        Just container -> void $ element container #+ [element runButton, element buildButton, element clearButton, element quitButton]
        Nothing -> return ()

    -- Add display area to the display container
    displayContainer <- getElementById window "threepenny-display-runner"
    case displayContainer of
        Just container -> void $ element container #+ [element displayArea]
        Nothing -> return ()

    -- Helper functions to update display (now using our Threepenny display area)
    let updateDisplay txt = element displayArea # set text txt
    let updateDisplayHtml htmlText = element displayArea # set html htmlText
    -- Set up the actual Haskell event handlers
    on UI.click runButton $ \_ -> do
        liftIO $ writeIORef outputRef []
        updateDisplay "Starting messageCollector..."
        
        -- First start messageCollector (background process, no UI output)
        liftIO $ runProcessSilent mcPhRef mcThreadRef mcHandleRef "cabal run messageCollector"
        
        -- Wait a moment for messageCollector to start, then start FsmRunner with live output
        liftIO $ forkIO $ do
            threadDelay 2000000 -- Wait 2 seconds for messageCollector to initialize
            runUI window $ void $ updateDisplay "Starting FsmRunner..."
            runProcessWithLiveOutput outputRef runnerPhRef runnerThreadRef runnerhandleRef window "cabal run FsmRunner" (\lines -> void $ runUI window (updateDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        
        return ()

    on UI.click buildButton $ \_ -> do
        liftIO $ writeIORef outputRef []
        updateDisplay "Building FSM..."
        liftIO $ runProcessWithLiveOutput outputRef runnerPhRef runnerThreadRef runnerhandleRef window "cabal run FsmBuilder" (\lines -> void $ runUI window (updateDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    on UI.click clearButton $ \_ -> do
        liftIO $ writeIORef outputRef []
        void $ updateDisplay ""

    on UI.click quitButton $ \_ -> do
        updateDisplay "Stopping processes..."
        -- Run cleanup in background thread to avoid blocking UI
        liftIO $ forkIO $ do
            -- Stop FsmRunner
            mph <- readIORef runnerPhRef
            mth <- readIORef runnerThreadRef
            mh  <- readIORef runnerhandleRef
            case mph of
              Just ph -> do
                terminateProcess ph
                putStrLn "Terminated FsmRunner process"
              Nothing -> putStrLn "No FsmRunner process to terminate"
            case mh of
              Just h -> hClose h
              Nothing -> return ()
            case mth of
              Just tid -> killThread tid
              Nothing -> return ()
            writeIORef runnerPhRef Nothing
            writeIORef runnerThreadRef Nothing
            writeIORef runnerhandleRef Nothing
            
            -- Stop messageCollector
            mcph <- readIORef mcPhRef
            mcth <- readIORef mcThreadRef
            mch  <- readIORef mcHandleRef
            case mcph of
              Just ph -> do
                terminateProcess ph
                putStrLn "Terminated messageCollector process"
              Nothing -> putStrLn "No messageCollector process to terminate"
            case mch of
              Just h -> hClose h
              Nothing -> return ()
            case mcth of
              Just tid -> killThread tid
              Nothing -> return ()
            writeIORef mcPhRef Nothing
            writeIORef mcThreadRef Nothing
            writeIORef mcHandleRef Nothing
            
            -- Exit the application
            exitSuccess
        return ()

-- Run process silently in background (no UI output)
runProcessSilent :: IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> String -> IO ()
runProcessSilent runnerPhRef runnerThreadRef runnerhandleRef cmd = do
    let proc = shell cmd
    (_, Just hout, _, ph) <- createProcess proc { std_out = CreatePipe }
    writeIORef runnerPhRef (Just ph)
    writeIORef runnerhandleRef (Just hout)
    hSetBuffering hout LineBuffering
    tid <- forkIO $ do
      let loop = do
            eof <- hIsEOF hout
            if eof
              then return ()
              else do
                _ <- hGetLine hout  -- Read and discard output
                loop
      loop
      _ <- waitForProcess ph
      writeIORef runnerPhRef Nothing
      writeIORef runnerThreadRef Nothing
      writeIORef runnerhandleRef Nothing
      return ()
    writeIORef runnerThreadRef (Just tid)
    return ()

runProcessWithLiveOutput :: IORef [String] -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> Window -> String -> ([String] -> IO ()) -> IO ()
runProcessWithLiveOutput outputRef runnerPhRef runnerThreadRef runnerhandleRef window cmd onLines = do
    let proc = shell cmd
    (_, Just hout, _, ph) <- createProcess proc { std_out = CreatePipe }
    writeIORef runnerPhRef (Just ph)
    writeIORef runnerhandleRef (Just hout)
    hSetBuffering hout LineBuffering
    tid <- forkIO $ do
      let loop = do
            eof <- hIsEOF hout
            if eof
              then return ()
              else do
                line <- hGetLine hout
                modifyIORef' outputRef (\ls -> ls ++ [line])
                lines <- readIORef outputRef
                onLines lines
                loop
      loop
      _ <- waitForProcess ph
      modifyIORef' outputRef (\ls -> ls ++ ["DONE"])
      lines <- readIORef outputRef
      onLines lines
      writeIORef runnerPhRef Nothing
      writeIORef runnerThreadRef Nothing
      writeIORef runnerhandleRef Nothing
      return ()
    writeIORef runnerThreadRef (Just tid)
    return ()

startFrontend :: IO ()
startFrontend = do
  outputRef <- newIORef []
  runnerPhRef <- newIORef Nothing
  runnerThreadRef <- newIORef Nothing
  runnerhandleRef <- newIORef Nothing
  mcPhRef <- newIORef Nothing
  mcThreadRef <- newIORef Nothing
  mcHandleRef <- newIORef Nothing
  startGUI defaultConfig { jsStatic = Just "./FsmFrontend/static" } (setup outputRef runnerPhRef runnerThreadRef runnerhandleRef mcPhRef mcThreadRef mcHandleRef)
