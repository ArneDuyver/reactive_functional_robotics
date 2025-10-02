module FsmFrontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Process (std_out, readProcess, readCreateProcess, shell, createProcess, StdStream(CreatePipe), waitForProcess, terminateProcess, ProcessHandle)
import System.IO (hSetBuffering, BufferMode(LineBuffering), hIsEOF, hGetLine, Handle, hClose)
import Control.Monad (void)
import Data.IORef
import System.Exit (exitSuccess)
import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay)

setup :: IORef [String] -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> Window -> UI ()
setup outputRef phRef threadRef handleRef mcPhRef mcThreadRef mcHandleRef window = do
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
    displayContainer <- getElementById window "threepenny-display"
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
            runProcessWithLiveOutput outputRef phRef threadRef handleRef window "cabal run FsmRunner" (\lines -> void $ runUI window (updateDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        
        return ()

    on UI.click buildButton $ \_ -> do
        liftIO $ writeIORef outputRef []
        updateDisplay "Building FSM..."
        liftIO $ runProcessWithLiveOutput outputRef phRef threadRef handleRef window "cabal run FsmBuilder" (\lines -> void $ runUI window (updateDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    on UI.click clearButton $ \_ -> do
        liftIO $ writeIORef outputRef []
        void $ updateDisplay ""

    on UI.click quitButton $ \_ -> do
        updateDisplay "Stopping processes..."
        -- Run cleanup in background thread to avoid blocking UI
        liftIO $ forkIO $ do
            -- Stop FsmRunner
            mph <- readIORef phRef
            mth <- readIORef threadRef
            mh  <- readIORef handleRef
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
            writeIORef phRef Nothing
            writeIORef threadRef Nothing
            writeIORef handleRef Nothing
            
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

-- Helper function to extract body content from HTML (no longer used but kept for compatibility)
extractBodyContent :: String -> String
extractBodyContent html = 
    let bodyStart = "<body>"
        bodyEnd = "</body>"
        startIndex = maybe 0 (+length bodyStart) (findSubstring bodyStart html)
        endIndex = maybe (length html) id (findSubstring bodyEnd html)
    in take (endIndex - startIndex) (drop startIndex html)

-- Simple substring finder
findSubstring :: String -> String -> Maybe Int
findSubstring needle haystack = findSubstringHelper needle haystack 0
  where
    findSubstringHelper _ [] _ = Nothing
    findSubstringHelper needle haystack@(_:rest) index
        | take (length needle) haystack == needle = Just index
        | otherwise = findSubstringHelper needle rest (index + 1)

-- Run process silently in background (no UI output)
runProcessSilent :: IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> String -> IO ()
runProcessSilent phRef threadRef handleRef cmd = do
    let proc = shell cmd
    (_, Just hout, _, ph) <- createProcess proc { std_out = CreatePipe }
    writeIORef phRef (Just ph)
    writeIORef handleRef (Just hout)
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
      writeIORef phRef Nothing
      writeIORef threadRef Nothing
      writeIORef handleRef Nothing
      return ()
    writeIORef threadRef (Just tid)
    return ()

runProcessWithLiveOutput :: IORef [String] -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> Window -> String -> ([String] -> IO ()) -> IO ()
runProcessWithLiveOutput outputRef phRef threadRef handleRef window cmd onLines = do
    let proc = shell cmd
    (_, Just hout, _, ph) <- createProcess proc { std_out = CreatePipe }
    writeIORef phRef (Just ph)
    writeIORef handleRef (Just hout)
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
      writeIORef phRef Nothing
      writeIORef threadRef Nothing
      writeIORef handleRef Nothing
      return ()
    writeIORef threadRef (Just tid)
    return ()

startFrontend :: IO ()
startFrontend = do
  outputRef <- newIORef []
  phRef <- newIORef Nothing
  threadRef <- newIORef Nothing
  handleRef <- newIORef Nothing
  mcPhRef <- newIORef Nothing
  mcThreadRef <- newIORef Nothing
  mcHandleRef <- newIORef Nothing
  startGUI defaultConfig { jsStatic = Just "./FsmFrontend/static" } (setup outputRef phRef threadRef handleRef mcPhRef mcThreadRef mcHandleRef)
