{-# LANGUAGE ScopedTypeVariables #-}
module FsmFrontendLinux where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Process (std_out, readProcess, readCreateProcess, shell, createProcess, StdStream(CreatePipe), waitForProcess, terminateProcess, ProcessHandle, getPid, getProcessExitCode)
import System.IO (hSetBuffering, BufferMode(LineBuffering), hIsEOF, hGetLine, Handle, hClose)
import Control.Monad (void, when, forM_)
import Data.IORef
import System.Exit (exitSuccess)
import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay)
import Control.Exception (catch, SomeException)
import System.FilePath (takeDirectory, (</>), takeBaseName, takeExtension)
import System.Directory (doesFileExist, removeFile, listDirectory, createDirectoryIfMissing, removeDirectoryRecursive, copyFile, doesDirectoryExist)
import Data.List (isSuffixOf)


-- outputRef: Stores accumulated output lines from running processes
-- PhRef: Reference to process handle for termination
-- ThreadRef: Reference to the thread reading output
-- handleRef: Reference to the stdout handle
-- window: The Threepenny-UI window object for the web interface
setup :: IORef [String] -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> IORef (Maybe ProcessHandle) -> IORef (Maybe ThreadId) -> IORef (Maybe Handle) -> Window -> UI ()
setup collectOutputRef runnerPhRef runnerThreadRef runnerhandleRef mcPhRef mcThreadRef mcHandleRef simPhRef simThreadRef simHandleRef window = do
    return window # set title "Reactive Functional Robotics"

    -- Load the homepage.html file and set it as the body content
    htmlContent <- liftIO $ readFile "./FsmFrontend/static/homepage.html"
    getBody window # set html htmlContent
    
    -- Wait a moment for DOM to be ready
    liftIO $ threadDelay 500000 -- 500ms to ensure DOM is loaded

    -- Get the gear button and upload button from HTML
    gearButton <- getElementById window "settings-btn"
    uploadButton <- getElementById window "upload-btn"
    importButton <- getElementById window "import-btn"
    exportButton <- getElementById window "export-btn"
    fileInput <- getElementById window "file-input"
    zipFileInput <- getElementById window "zip-file-input"
    pythonPathInput <- getElementById window "python-path-input"

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
    
    -- Helper functions that return UI () for cleanup
    let updateCollectDisplayVoid txt = void $ updateCollectDisplay txt
    let updateRunDisplayVoid txt = void $ updateRunDisplay txt
    let updateSimulationDisplayVoid txt = void $ updateSimulationDisplay txt
    
    let setButtonActive btnElement = element btnElement # set (attr "class") "btn btn-process active"
    let setButtonInactive btnElement = element btnElement # set (attr "class") "btn btn-process"
    
    -- Create separate output references for each process to avoid conflicts
    runnerOutputRef <- liftIO $ newIORef []
    buildOutputRef <- liftIO $ newIORef []
    simOutputRef <- liftIO $ newIORef []
    
    -- Set up the event handlers for each button
    on UI.click collectButton $ \_ -> do
        setButtonActive collectButton
        updateCollectDisplay "Starting messageCollector..."
        liftIO $ runProcessWithLiveOutput collectOutputRef mcPhRef mcThreadRef mcHandleRef window "cabal run exe:messagecollector" 
            (\lines -> void $ runUI window (updateCollectDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    on UI.click runButton $ \_ -> do
        setButtonActive runButton
        updateRunDisplay "Starting FsmRunner..."
        liftIO $ runProcessWithLiveOutput runnerOutputRef runnerPhRef runnerThreadRef runnerhandleRef window "cabal run exe:fsmrunner" 
            (\lines -> void $ runUI window (updateRunDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    on UI.click buildButton $ \_ -> do
        setButtonActive buildButton
        updateBuildDisplay "Building FSM..."
        liftIO $ runProcessWithLiveOutput buildOutputRef runnerPhRef runnerThreadRef runnerhandleRef window "cabal run FsmBuilder" 
            (\lines -> void $ runUI window (updateBuildDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    on UI.click simulationButton $ \_ -> do
        setButtonActive simulationButton
        updateSimulationDisplay "Starting simulation..."
        -- Get the Python path from the input field
        pythonPath <- case pythonPathInput of
            Just input -> get value input
            Nothing -> return "../complete.py"
        let command = "sudo -n ./config/root-scripts/process-helper.sh start-simulation \"" ++ pythonPath ++ "\""
        liftIO $ runProcessWithLiveOutput simOutputRef simPhRef simThreadRef simHandleRef window command
            (\lines -> void $ runUI window (updateSimulationDisplayHtml (unlines (map (\l -> "<div>" ++ l ++ "</div>") lines))))
        return ()

    on UI.click clearButton $ \_ -> do
        liftIO $ writeIORef collectOutputRef []
        liftIO $ writeIORef runnerOutputRef []
        liftIO $ writeIORef buildOutputRef []
        liftIO $ writeIORef simOutputRef []
        void $ updateCollectDisplay ""
        void $ updateRunDisplay ""
        void $ updateBuildDisplay ""
        void $ updateSimulationDisplay ""
        -- Reset button states
        setButtonInactive collectButton
        setButtonInactive runButton
        setButtonInactive buildButton
        setButtonInactive simulationButton

    -- Event handler for gear button - opens states folder
    case gearButton of
        Just btn -> on UI.click btn $ \_ -> do
            liftIO $ do
                catch (do
                  _ <- readProcess "xdg-open" ["FsmRunner/src/Helpers/States"] ""
                  return ())
                  (\e -> putStrLn $ "Failed to open folder: " ++ show (e :: SomeException))
            return ()
        Nothing -> return ()

    -- Event handler for upload button - opens config folder
    case uploadButton of
        Just btn -> on UI.click btn $ \_ -> do
            liftIO $ do
                putStrLn "Opening config folder..."
                _ <- createProcess (shell "xdg-open config")
                return ()
        Nothing -> return ()

    -- Event handler for import button - import project from zip
    case importButton of
        Just btn -> on UI.click btn $ \_ -> do
            updateBuildDisplay "Starting import process..."
            liftIO $ importProject window (\html -> void $ updateBuildDisplayHtml html)
        Nothing -> return ()

    -- Event handler for export button - export project to zip
    case exportButton of
        Just btn -> on UI.click btn $ \_ -> do
            updateBuildDisplay "Starting export process..."
            liftIO $ exportProject window (\html -> void $ updateBuildDisplayHtml html)
        Nothing -> return ()

    on UI.click quitButton $ \_ -> do
        updateCollectDisplay "Stopping processes..."
        -- Reset all button states to inactive
        setButtonInactive collectButton
        setButtonInactive runButton
        setButtonInactive buildButton
        setButtonInactive simulationButton
        -- Run cleanup in background thread to avoid blocking UI
        liftIO $ forkIO $ do
            -- Read all process references first before cleanup
            mph <- readIORef runnerPhRef
            mth <- readIORef runnerThreadRef
            mh  <- readIORef runnerhandleRef
            mcph <- readIORef mcPhRef
            mcth <- readIORef mcThreadRef
            mch  <- readIORef mcHandleRef
            simph <- readIORef simPhRef
            simth <- readIORef simThreadRef
            simh  <- readIORef simHandleRef
            
            -- Simple cleanup for all processes
            -- Stop FsmRunner with graceful shutdown first
            case mph of
              Just ph -> do
                -- Create stop signal file for graceful shutdown
                catch (do
                  writeFile "FsmRunner.stop" ""
                  putStrLn "Created FsmRunner stop signal, waiting for graceful shutdown..."
                  return ())
                  (\e -> putStrLn $ "Failed to create FsmRunner stop signal: " ++ show (e :: SomeException))
                -- Wait a bit for graceful shutdown
                threadDelay 2000000 -- 2 seconds
                -- Check if still running, then force terminate with kill
                mExitCode <- getProcessExitCode ph
                case mExitCode of
                  Nothing -> do -- Still running, force terminate with kill -9
                    mpid <- getPid ph
                    case mpid of
                      Just pid -> do
                        putStrLn $ "FsmRunner still running after graceful shutdown attempt, using kill -9 on PID: " ++ show pid
                        catch (do
                          _ <- readProcess "sudo"
                               ["-n", "./config/root-scripts/process-helper.sh", "kill-fsmrunner", show pid]
                               ""
                          putStrLn "Force termination with kill -9 completed"
                          return ())
                          (\e -> do
                            putStrLn $ "kill failed: " ++ show (e :: SomeException)
                            putStrLn "Falling back to terminateProcess"
                            terminateProcess ph)
                      Nothing -> do
                        putStrLn "Could not get process ID, using terminateProcess"
                        terminateProcess ph
                  Just _ -> putStrLn "FsmRunner stopped gracefully"
                -- Clean up stop signal file
                catch (removeFile "FsmRunner.stop") (\(_ :: SomeException) -> return ())
                void $ runUI window (updateRunDisplayVoid "FsmRunner process terminated")
              Nothing -> void $ runUI window (updateRunDisplayVoid "No FsmRunner process to terminate")
            case mh of
              Just h -> catch (hClose h) (\(_ :: SomeException) -> return ())
              Nothing -> return ()
            case mth of
              Just tid -> killThread tid
              Nothing -> return ()
            writeIORef runnerPhRef Nothing
            writeIORef runnerThreadRef Nothing
            writeIORef runnerhandleRef Nothing
            
            -- Stop messageCollector with graceful shutdown
            case mcph of
              Just ph -> do
                -- Create stop signal file for graceful shutdown
                catch (do
                  writeFile "messageCollector.stop" ""
                  putStrLn "Created messageCollector stop signal, waiting for graceful shutdown..."
                  return ())
                  (\e -> putStrLn $ "Failed to create messageCollector stop signal: " ++ show (e :: SomeException))
                -- Wait a bit for graceful shutdown
                threadDelay 2000000 -- 2 seconds
                -- Check if still running, then force terminate with kill
                mExitCode <- getProcessExitCode ph
                case mExitCode of
                  Nothing -> do -- Still running, force terminate with kill -9
                    mpid <- getPid ph
                    case mpid of
                      Just pid -> do
                        putStrLn $ "MessageCollector still running after graceful shutdown attempt, using kill -9 on PID: " ++ show pid
                        catch (do
                          _ <- readProcess "sudo"
                               ["-n", "./config/root-scripts/process-helper.sh", "kill-messagecollector", show pid]
                               ""
                          putStrLn "Force termination with kill -9 completed"
                          return ())
                          (\e -> do
                            putStrLn $ "kill failed: " ++ show (e :: SomeException)
                            putStrLn "Falling back to terminateProcess"
                            terminateProcess ph)
                      Nothing -> do
                        putStrLn "Could not get process ID, using terminateProcess"
                        terminateProcess ph
                  Just _ -> putStrLn "MessageCollector stopped gracefully"
                -- Clean up stop signal file
                catch (removeFile "messageCollector.stop") (\(_ :: SomeException) -> return ())
                void $ runUI window (updateCollectDisplayVoid "messageCollector process terminated")
              Nothing -> void $ runUI window (updateCollectDisplayVoid "No messageCollector process to terminate")
            case mch of
              Just h -> catch (hClose h) (\(_ :: SomeException) -> return ())
              Nothing -> return ()
            case mcth of
              Just tid -> killThread tid
              Nothing -> return ()
            writeIORef mcPhRef Nothing
            writeIORef mcThreadRef Nothing
            writeIORef mcHandleRef Nothing
            
            -- Stop simulation with graceful shutdown first
            case simph of
              Just ph -> do
                putStrLn "Creating stop.flag file for graceful shutdown..."
                -- Get the Python path from the input field (or use default)
                pythonPath <- case pythonPathInput of
                  Just input -> runUI window $ get value input
                  Nothing -> return "../complete.py"
                
                -- Extract directory from Python file path and create stop.flag there
                let pythonDir = takeDirectory pythonPath
                let flagFile = pythonDir ++ "/stop.flag"
                
                catch (do
                  writeFile flagFile ""
                  putStrLn $ "Created stop.flag at: " ++ flagFile
                  return ())
                  (\e -> putStrLn $ "Failed to create stop.flag: " ++ show (e :: SomeException))
                
                -- Wait for graceful shutdown (5 seconds)
                putStrLn "Waiting for graceful shutdown (5 seconds)..."
                threadDelay 5000000 -- 5 seconds
                
                -- Check if process is still running after graceful shutdown attempt
                mExitCode <- getProcessExitCode ph
                case mExitCode of
                  Nothing -> do -- Still running, force terminate with kill -9
                    mpid <- getPid ph
                    case mpid of
                      Just pid -> do
                        putStrLn $ "Process still running after graceful shutdown attempt, using kill -9 on PID: " ++ show pid
                        catch (do
                          _ <- readProcess "sudo"
                               ["-n", "./config/root-scripts/process-helper.sh", "kill-simulation", show pid]
                               ""
                          putStrLn "Force termination with kill -9 completed"
                          return ())
                          (\e -> do
                            putStrLn $ "kill failed: " ++ show (e :: SomeException)
                            putStrLn "Falling back to terminateProcess"
                            terminateProcess ph)
                      Nothing -> do
                        putStrLn "Could not get process ID, using terminateProcess"
                        terminateProcess ph
                  Just _ -> putStrLn "Process exited gracefully"
                
                void $ runUI window (updateSimulationDisplayVoid "Simulation process terminated \nNow stop the simulation in the simulator")
              Nothing -> void $ runUI window (updateSimulationDisplayVoid "No simulation process to terminate")
            case simh of
              Just h -> catch (hClose h) (\(_ :: SomeException) -> return ())
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
runProcessWithLiveOutput outputRef processPhRef processThreadRef processHandleRef window cmd onLines = do
    -- Simple cleanup: just kill existing thread and close handle if they exist
    existingTh <- readIORef processThreadRef
    case existingTh of
      Just tid -> killThread tid
      Nothing -> return ()
    
    existingH <- readIORef processHandleRef
    case existingH of
      Just h -> catch (hClose h) (\(_ :: SomeException) -> return ())
      Nothing -> return ()
    
    -- Clear output and start fresh
    writeIORef outputRef []
    
    -- Start new process
    let proc = shell cmd
    (_, Just hout, _, ph) <- createProcess proc { std_out = CreatePipe }
    writeIORef processPhRef (Just ph)
    writeIORef processHandleRef (Just hout)
    hSetBuffering hout LineBuffering
    
    tid <- forkIO $ do
      let loop = do
            eof <- hIsEOF hout
            if eof
              then return ()
              else do
                line <- catch (hGetLine hout) (\(_ :: SomeException) -> return "")
                when (line /= "") $ do
                  modifyIORef' outputRef (\ls -> take 50 (ls ++ [line])) -- Keep only last 50 lines
                  lines <- readIORef outputRef
                  onLines lines
                loop
      loop
      -- Process finished, clean up
      catch (hClose hout) (\(_ :: SomeException) -> return ())
      writeIORef processPhRef Nothing
      writeIORef processThreadRef Nothing
      writeIORef processHandleRef Nothing
      -- Signal completion
      modifyIORef' outputRef (\ls -> ls ++ ["Process completed"])
      lines <- readIORef outputRef
      onLines lines
      return ()
    
    writeIORef processThreadRef (Just tid)
    return ()

-- Import project from zip file
importProject :: Window -> (String -> UI ()) -> IO ()
importProject window updateDisplay = do
    putStrLn "Starting import process..."
    void $ runUI window $ updateDisplay "<div>Starting import process...</div>"
    
    catch (do
        -- Use zenity for file dialog on Linux
        result <- readProcess "zenity" ["--file-selection", 
            "--title=Select Project ZIP File",
            "--file-filter=ZIP files (*.zip) | *.zip",
            "--filename=./saved_projects/"] ""
        
        let zipPath = init result -- Remove trailing newline
        
        if null zipPath
            then do
                putStrLn "Import cancelled by user"
                void $ runUI window $ updateDisplay "<div>Import cancelled by user</div>"
            else do
                putStrLn $ "Selected zip file: " ++ zipPath
                void $ runUI window $ updateDisplay $ "<div>Selected zip file: " ++ zipPath ++ "</div><div>Extracting...</div>"
                
                -- Create temporary extraction directory
                let tempDir = "temp_import"
                createDirectoryIfMissing True tempDir
                
                -- Extract zip file using unzip
                _ <- readProcess "unzip" ["-o", zipPath, "-d", tempDir] ""
                
                -- Check if extracted files exist
                tempContents <- listDirectory tempDir
                putStrLn $ "Extracted contents: " ++ show tempContents
                
                void $ runUI window $ updateDisplay $ "<div>Extracted contents: " ++ show tempContents ++ "</div><div>Copying files...</div>"
                
                -- Copy files to their destinations
                -- 1. Copy fsm.xml
                let fsmXmlSource = tempDir </> "fsm.xml"
                let fsmXmlDest = "config" </> "fsm.xml"
                fsmXmlExists <- doesFileExist fsmXmlSource
                if fsmXmlExists
                    then do
                        copyFile fsmXmlSource fsmXmlDest
                        putStrLn "Copied fsm.xml to config folder"
                    else putStrLn "Warning: fsm.xml not found in zip file"
                
                -- 2. Copy configuration.yml
                let configYmlSource = tempDir </> "configuration.yml"
                let configYmlDest = "config" </> "configuration.yml"
                configYmlExists <- doesFileExist configYmlSource
                if configYmlExists
                    then do
                        copyFile configYmlSource configYmlDest
                        putStrLn "Copied configuration.yml to config folder"
                    else putStrLn "Warning: configuration.yml not found in zip file"
                
                -- 3. Clear existing states folder and copy new states
                let statesDir = "FsmRunner" </> "src" </> "Helpers" </> "States"
                let tempStatesDir = tempDir </> "states"
                
                tempStatesDirExists <- doesDirectoryExist tempStatesDir
                if tempStatesDirExists
                    then do
                        -- Remove existing states
                        existingStates <- catch (listDirectory statesDir) (\(_ :: SomeException) -> return [])
                        forM_ existingStates $ \stateFile -> do
                            if ".hs" `isSuffixOf` stateFile
                                then catch (removeFile (statesDir </> stateFile)) (\(_ :: SomeException) -> return ())
                                else return ()
                        
                        -- Copy new states
                        newStates <- catch (listDirectory tempStatesDir) (\(_ :: SomeException) -> return [])
                        forM_ newStates $ \stateFile -> do
                            if ".hs" `isSuffixOf` stateFile
                                then do
                                    copyFile (tempStatesDir </> stateFile) (statesDir </> stateFile)
                                    putStrLn $ "Copied state file: " ++ stateFile
                                else return ()
                        putStrLn "Replaced all state files"
                    else putStrLn "Warning: states folder not found in zip file"
                
                -- Clean up temporary directory
                removeDirectoryRecursive tempDir
                putStrLn "Import completed successfully!"
                
                void $ runUI window $ updateDisplay "<div style='color: green;'>Import completed successfully!</div><div>Files have been imported to their respective locations.</div>"
                
        ) (\e -> do
            putStrLn $ "Import failed: " ++ show (e :: SomeException)
            void $ runUI window $ updateDisplay $ "<div style='color: red;'>Import failed: " ++ show (e :: SomeException) ++ "</div>"
            )

-- Export project to zip file
exportProject :: Window -> (String -> UI ()) -> IO ()
exportProject window updateDisplay = do
    putStrLn "Starting export process..."
    void $ runUI window $ updateDisplay "<div>Starting export process...</div>"
    
    catch (do
        -- Use zenity for input dialog on Linux
        result <- readProcess "zenity" ["--entry",
            "--title=Export Project",
            "--text=Enter project name:",
            "--entry-text=MyProject"] ""
        
        let projectName = init result -- Remove trailing newline
        
        if null projectName
            then do
                putStrLn "Export cancelled by user"
                void $ runUI window $ updateDisplay "<div>Export cancelled by user</div>"
            else do
                putStrLn $ "Project name: " ++ projectName
                void $ runUI window $ updateDisplay $ "<div>Project name: " ++ projectName ++ "</div><div>Preparing export...</div>"
                
                -- Create temporary directory for export
                let tempDir = "temp_export"
                createDirectoryIfMissing True tempDir
                
                -- Copy files to temporary directory
                -- 1. Copy fsm.xml
                let fsmXmlSource = "config" </> "fsm.xml"
                let fsmXmlDest = tempDir </> "fsm.xml"
                fsmXmlExists <- doesFileExist fsmXmlSource
                if fsmXmlExists
                    then do
                        copyFile fsmXmlSource fsmXmlDest
                        putStrLn "Added fsm.xml to export"
                    else putStrLn "Warning: fsm.xml not found"
                
                -- 2. Copy configuration.yml
                let configYmlSource = "config" </> "configuration.yml"
                let configYmlDest = tempDir </> "configuration.yml"
                configYmlExists <- doesFileExist configYmlSource
                if configYmlExists
                    then do
                        copyFile configYmlSource configYmlDest
                        putStrLn "Added configuration.yml to export"
                    else putStrLn "Warning: configuration.yml not found"
                
                -- 3. Copy states folder
                let statesDir = "FsmRunner" </> "src" </> "Helpers" </> "States"
                let tempStatesDir = tempDir </> "states"
                createDirectoryIfMissing True tempStatesDir
                
                void $ runUI window $ updateDisplay $ "<div>Project name: " ++ projectName ++ "</div><div>Copying state files...</div>"
                
                stateFiles <- catch (listDirectory statesDir) (\(_ :: SomeException) -> return [])
                forM_ stateFiles $ \stateFile -> do
                    if ".hs" `isSuffixOf` stateFile
                        then do
                            catch (copyFile (statesDir </> stateFile) (tempStatesDir </> stateFile)) 
                                (\e -> putStrLn $ "Failed to copy " ++ stateFile ++ ": " ++ show (e :: SomeException))
                            putStrLn $ "Added state file: " ++ stateFile
                        else return ()
                
                -- Create zip file in saved_projects directory
                let zipFileName = projectName ++ ".zip"
                let zipPath = "saved_projects" </> zipFileName
                
                void $ runUI window $ updateDisplay $ "<div>Project name: " ++ projectName ++ "</div><div>Creating ZIP file...</div>"
                
                -- Create zip using zip command
                _ <- readProcess "bash" ["-c", "cd " ++ tempDir ++ " && zip -r ../" ++ zipPath ++ " *"] ""
                
                -- Clean up temporary directory
                removeDirectoryRecursive tempDir
                
                putStrLn $ "Project exported successfully to: " ++ zipPath
                
                void $ runUI window $ updateDisplay $ "<div style='color: green;'>Project exported successfully!</div><div>Saved to: " ++ zipPath ++ "</div><div>Opening saved_projects folder...</div>"
                
                -- Open saved_projects folder with default file manager
                _ <- createProcess (shell "xdg-open saved_projects")
                return ()
                
        ) (\e -> do
            putStrLn $ "Export failed: " ++ show (e :: SomeException)
            void $ runUI window $ updateDisplay $ "<div style='color: red;'>Export failed: " ++ show (e :: SomeException) ++ "</div>"
            )

startFrontendLinux :: IO ()
startFrontendLinux = do
  collectOutputRef <- newIORef []
  runnerPhRef <- newIORef Nothing
  runnerThreadRef <- newIORef Nothing
  runnerhandleRef <- newIORef Nothing
  mcPhRef <- newIORef Nothing
  mcThreadRef <- newIORef Nothing
  mcHandleRef <- newIORef Nothing
  simPhRef <- newIORef Nothing
  simThreadRef <- newIORef Nothing
  simHandleRef <- newIORef Nothing
  startGUI defaultConfig { jsStatic = Just "./FsmFrontend/static" } (setup collectOutputRef runnerPhRef runnerThreadRef runnerhandleRef mcPhRef mcThreadRef mcHandleRef simPhRef simThreadRef simHandleRef)
