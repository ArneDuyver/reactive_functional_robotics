module FsmFrontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Process (std_out, readProcess, readCreateProcess, shell, createProcess, StdStream(CreatePipe), waitForProcess, terminateProcess, ProcessHandle)
import System.IO (hSetBuffering, BufferMode(LineBuffering), hIsEOF, hGetLine, Handle, hClose)
import Control.Monad (void)
import Data.IORef
import System.Exit (exitSuccess)
import Control.Concurrent (forkIO, killThread, ThreadId)

setup :: IORef [String] -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> Window -> UI ()
setup outputRef phRef threadRef handleRef window = do
    return window # set title "Threepenny Counter"

    -- Create UI elements
    myLabel <- UI.h1 # set text "FSM"
                     # set style [("background-color","yellow"),("color", "red")]
    myButton <- UI.button # set text "Run"
    buildButton <- UI.button # set text "Build"
    clearButton <- UI.button # set text "Clear"
    quitButton <- UI.button # set text "Quit"
    display <- UI.div # set text "Press a button to start ..."

    -- Custom HTML block with <script>
    customHtml <- UI.div # set UI.html "<h1>Mermaid & MQTT Client</h1><div id='mermaid-container' class='fade-in'>Loading...</div>"

    -- Layout
    getBody window #+ [column [element myLabel, row [element myButton, element buildButton, element clearButton, element quitButton], element display, element customHtml]]

    -- Load external JS file (served from /static/mermaid-mqtt.js)
    void $ getHead window #+ [mkElement "script" # set (attr "src") "/static/mermaid-mqtt.js"]

    -- Button click event handler TODO: make it run the FsmRunner 
    on UI.click myButton $ \_ -> do
        liftIO $ writeIORef outputRef []
        void $ element display # set text ""
        -- liftIO $ runProcessWithLiveOutput outputRef phRef threadRef handleRef window ".\\FsmFrontend\\test\\myprogram.exe" (\lines -> void $ runUI window (element display # set html (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    -- Build button event handler TODO: make it run the FsmBuilder FIXME: add "cabal clean ; cabal build ;" 
    on UI.click buildButton $ \_ -> do
        liftIO $ writeIORef outputRef []
        void $ element display # set text ""
        liftIO $ runProcessWithLiveOutput outputRef phRef threadRef handleRef window "cabal run FsmBuilder" (\lines -> void $ runUI window (element display # set html (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        -- liftIO $ runProcessWithLiveOutput outputRef phRef threadRef handleRef window "ghc -o FsmFrontend/test/myprogram FsmFrontend/test/program.hs" (\lines -> void $ runUI window (element display # set html (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    -- Clear button event handler
    on UI.click clearButton $ \_ -> do
        liftIO $ writeIORef outputRef []
        void $ element display # set text ""

    -- Quit button event handler
    on UI.click quitButton $ \_ -> do
        mph <- liftIO $ readIORef phRef
        mth <- liftIO $ readIORef threadRef
        mh  <- liftIO $ readIORef handleRef
        case mph of
          Just ph -> liftIO $ terminateProcess ph
          Nothing -> return ()
        case mh of
          Just h -> liftIO $ hClose h
          Nothing -> return ()
        case mth of
          Just tid -> liftIO $ killThread tid
          Nothing -> return ()
        liftIO $ writeIORef phRef Nothing
        liftIO $ writeIORef threadRef Nothing
        liftIO $ writeIORef handleRef Nothing
        liftIO exitSuccess
        return ()

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
  startGUI defaultConfig { jsStatic = Just "./FsmFrontend/static" } (setup outputRef phRef threadRef handleRef)
