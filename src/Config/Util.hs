module Config.Util
  ( getConfigDir
  , run
  , run'
  ) where

import Control.Monad (void)
import System.Directory
import System.Posix.Env (getEnv)
import System.Process
import XMonad hiding (recompile, restart)

run :: MonadIO m => CreateProcess -> m ()
run = void . liftIO . createProcess

run' :: MonadIO m => [String] -> m ()
run' c = run . proc (head c) $ tail c

getConfigDir :: IO FilePath
getConfigDir = do
  let xdg = getXdgDirectory XdgConfig "xmonad"
  envPath <- getEnv "XMONAD_CONFIG_DIR"
  case envPath of
    Nothing -> xdg
    Just p -> do
      envExists <- doesPathExist p
      if envExists
        then return p
        else xdg
