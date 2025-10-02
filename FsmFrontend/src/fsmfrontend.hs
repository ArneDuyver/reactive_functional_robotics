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
setup :: IORef [String] -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> Window -> UI ()
setup outputRef runnerPhRef runnerThreadRef runnerhandleRef mcPhRef mcThreadRef mcHandleRef simPhRef simThreadRef simHandleRef window = do
    return window # set title "Reactive Functional Robotics"

    -- Load the homepage.html file and set it as the body content
    htmlContent <- liftIO $ readFile "./FsmFrontend/static/homepage.html"
    getBody window # set html htmlContent
    
    -- Wait a moment for DOM to be ready
    liftIO $ threadDelay 500000 -- 500ms to ensure DOM is loaded

    -- Create Threepenny buttons with proper CSS classes and IDs
    collectButton <- UI.button # set text "Collect" 
                              # set (attr "class") "btn btn-process"
                              # set (attr "id") "collect-btn"
    runButton <- UI.button # set text "Run" 
                          # set (attr "class") "btn btn-process"
                          # set (attr "id") "run-btn"
    buildButton <- UI.button # set text "Build"
                            # set (attr "class") "btn btn-process"
                            # set (attr "id") "build-btn"
    simulationButton <- UI.button # set text "Simulation"
                                 # set (attr "class") "btn btn-process"
                                 # set (attr "id") "simulation-btn"
    clearButton <- UI.button # set text "Clear"
                            # set (attr "class") "btn btn-clear"
    quitButton <- UI.button # set text "Quit"
                           # set (attr "class") "btn btn-quit"

    -- Create Threepenny display areas for each tab
    collectDisplayArea <- UI.div # set (attr "class") "display-area"
                                # set text "Press Collect to start message collector..."
    runDisplayArea <- UI.div # set (attr "class") "display-area"
                            # set text "Press Run to start FSM Runner..."
    buildDisplayArea <- UI.div # set (attr "class") "display-area"
                              # set text "Press Build to start FSM Builder..."
    simulationDisplayArea <- UI.div # set (attr "class") "display-area"
                                   # set text "Press Simulation to start simulation..."

    -- Add buttons to the button container
    buttonContainer <- getElementById window "threepenny-buttons"
    case buttonContainer of
        Just container -> void $ element container #+ [element collectButton, element runButton, element buildButton, element simulationButton, element clearButton, element quitButton]
        Nothing -> return ()

    -- Add display areas to their corresponding containers
    collectContainer <- getElementById window "threepenny-display-messagecollector"
    case collectContainer of
        Just container -> void $ element container #+ [element collectDisplayArea]
        Nothing -> return ()
        
    runContainer <- getElementById window "threepenny-display-runner"
    case runContainer of
        Just container -> void $ element container #+ [element runDisplayArea]
        Nothing -> return ()
        
    buildContainer <- getElementById window "threepenny-display-builder"
    case buildContainer of
        Just container -> void $ element container #+ [element buildDisplayArea]
        Nothing -> return ()
        
    simulationContainer <- getElementById window "threepenny-display-simulation"
    case simulationContainer of
        Just container -> void $ element container #+ [element simulationDisplayArea]
        Nothing -> return ()

    -- Helper functions to update displays and button states
    let updateCollectDisplay txt = element collectDisplayArea # set text txt
    let updateRunDisplay txt = element runDisplayArea # set text txt
    let updateBuildDisplay txt = element buildDisplayArea # set text txt
    let updateSimulationDisplay txt = element simulationDisplayArea # set text txt
    let updateCollectDisplayHtml htmlText = element collectDisplayArea # set html htmlText
    let updateRunDisplayHtml htmlText = element runDisplayArea # set html htmlText
    let updateBuildDisplayHtml htmlText = element buildDisplayArea # set html htmlText
    let updateSimulationDisplayHtml htmlText = element simulationDisplayArea # set html htmlText
    
    let setButtonActive btnElement = element btnElement # set (attr "class") "btn btn-process active"
    let setButtonInactive btnElement = element btnElement # set (attr "class") "btn btn-process"
    
    -- Set up the event handlers for each button
    on UI.click collectButton $ \_ -> do
        setButtonActive collectButton
        updateCollectDisplay "Starting messageCollector..."
        liftIO $ runProcessWithLiveOutput outputRef mcPhRef mcThreadRef mcHandleRef window "cabal run messageCollector" 
            (\lines -> void $ runUI window (updateCollectDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    on UI.click runButton $ \_ -> do
        setButtonActive runButton
        updateRunDisplay "Starting FsmRunner..."
        liftIO $ runProcessWithLiveOutput outputRef runnerPhRef runnerThreadRef runnerhandleRef window "cabal run FsmRunner" 
            (\lines -> void $ runUI window (updateRunDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    on UI.click buildButton $ \_ -> do
        setButtonActive buildButton
        updateBuildDisplay "Building FSM..."
        liftIO $ runProcessWithLiveOutput outputRef runnerPhRef runnerThreadRef runnerhandleRef window "cabal run FsmBuilder" 
            (\lines -> void $ runUI window (updateBuildDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    on UI.click simulationButton $ \_ -> do
        setButtonActive simulationButton
        updateSimulationDisplay "Starting simulation..."
        liftIO $ runProcessWithLiveOutput outputRef simPhRef simThreadRef simHandleRef window "python \"C:\\Users\\Work\\OneDrive - KU Leuven\\Documents\\PhD\\Code\\1_Research\\1_turtlebot_copp_programs\\main.py\"" 
            (\lines -> void $ runUI window (updateSimulationDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    on UI.click clearButton $ \_ -> do
        liftIO $ writeIORef outputRef []
        void $ updateCollectDisplay ""
        void $ updateRunDisplay ""
        void $ updateBuildDisplay ""
        void $ updateSimulationDisplay ""

    on UI.click quitButton $ \_ -> do
        updateCollectDisplay "Stopping processes..."
        -- Reset all button states to inactive
        setButtonInactive collectButton
        setButtonInactive runButton
        setButtonInactive buildButton
        setButtonInactive simulationButton
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
            
            -- Stop simulation
            simph <- readIORef simPhRef
            simth <- readIORef simThreadRef
            simh  <- readIORef simHandleRef
            case simph of
              Just ph -> do
                terminateProcess ph
                putStrLn "Terminated simulation process"
              Nothing -> putStrLn "No simulation process to terminate"
            case simh of
              Just h -> hClose h
              Nothing -> return ()
            case simth of
              Just tid -> killThread tid
              Nothing -> return ()
            writeIORef simPhRef Nothing
            writeIORef simThreadRef Nothing
            writeIORef simHandleRef Nothing
            
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
  simPhRef <- newIORef Nothing
  simThreadRef <- newIORef Nothing
  simHandleRef <- newIORef Nothing
  startGUI defaultConfig { jsStatic = Just "./FsmFrontend/static" } (setup outputRef runnerPhRef runnerThreadRef runnerhandleRef mcPhRef mcThreadRef mcHandleRef simPhRef simThreadRef simHandleRef)
