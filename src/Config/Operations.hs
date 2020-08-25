module Config.Operations
  ( recompile
  , restart
  , recompileRestart
  , sendRestart
  , sendReplace
  ) where

import Config.Terminal
import Config.Util
import Control.Monad (when)
import Data.Function (fix)
import System.Directory
import System.Environment
import System.Exit
import System.Process
import Text.Printf (printf)
import XMonad hiding (recompile, restart)
import qualified XMonad.Operations as O

recompile :: Maybe Terminal -> IO Bool
recompile term = do
  dir <- getConfigDir
  exists <- doesPathExist dir
  uninstallSignalHandlers
  let compileConsole :: IO Bool
      compileConsole = do
        putStrLn . printf "Recompiling the source code at %s." $ dir
        (_, _, _, h) <- createProcess $ recompileProcess dir
        status <- waitForProcess h
        if status == ExitSuccess
          then putStrLn "XMonad recompilation exited with success." >>
               return True
          else putStrLn "XMonad recompilation exited with failure." >>
               return False
  let compileTerminal :: Terminal -> IO Bool
      compileTerminal t = do
        (status, _, err) <-
          readCreateProcessWithExitCode (recompileProcess dir) []
        if status == ExitSuccess
          then return True
          else printToTerminal t err >> return False
  res <-
    if exists
      then maybe compileConsole compileTerminal term
      else do
        putStrLn . printf "%s doesn't exist." $ dir
        return False
  installSignalHandlers
  return res
  where
    recompileProcess :: FilePath -> CreateProcess
    recompileProcess dir = (proc "stack" ["install"]) {cwd = Just dir}

restart :: X ()
restart = liftIO getProgName >>= (`O.restart` True)

recompileRestart :: Terminal -> X ()
recompileRestart t = liftIO (recompile . Just $ t) >>= (`when` restart)

-- Following three functions are copied from XMonad source code.
sendRestart :: IO ()
sendRestart = do
  dpy <- openDisplay ""
  rw <- rootWindow dpy $ defaultScreen dpy
  restartAtom <- internAtom dpy "WINDOW_MANAGER_RESTART" False
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e rw restartAtom 32 0 currentTime
    sendEvent dpy rw False structureNotifyMask e
  sync dpy False

sendReplace :: IO ()
sendReplace = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw <- rootWindow dpy dflt
  replace dpy dflt rootw

replace :: Display -> ScreenNumber -> Window -> IO ()
replace dpy dflt rootw = do
  wmSnAtom <- internAtom dpy ("WM_S" ++ show dflt) False
  currentWmSnOwner <- xGetSelectionOwner dpy wmSnAtom
  when (currentWmSnOwner /= 0) $ do
    selectInput dpy currentWmSnOwner structureNotifyMask
    netWmSnOwner <-
      allocaSetWindowAttributes $ \attributes -> do
        set_override_redirect attributes True
        set_event_mask attributes propertyChangeMask
        let screen = defaultScreenOfDisplay dpy
            visual = defaultVisualOfScreen screen
            attrmask = cWOverrideRedirect .|. cWEventMask
        createWindow
          dpy
          rootw
          (-100)
          (-100)
          1
          1
          0
          copyFromParent
          copyFromParent
          visual
          attrmask
          attributes
    xSetSelectionOwner dpy wmSnAtom netWmSnOwner currentTime
    fix $ \again -> do
      evt <-
        allocaXEvent $ \event -> do
          windowEvent dpy currentWmSnOwner structureNotifyMask event
          get_EventType event
      when (evt /= destroyNotify) again
