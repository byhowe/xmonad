module Config.Util
  ( fork
  , forkPID
  , run
  , run'
  , getScreens
  , generateRandomString
  ) where

import Control.Monad (void)
import Graphics.X11.Xinerama (getScreenInfo)
import System.Posix (ProcessID)
import System.Process
import System.Random (newStdGen, randoms)
import XMonad hiding (recompile, restart)

forkPID :: MonadIO m => IO a -> m ProcessID
forkPID = xfork . void

fork :: MonadIO m => IO a -> m ()
fork = void . forkPID

run :: MonadIO m => CreateProcess -> m ()
run = fork . createProcess

run' :: MonadIO m => [String] -> m ()
run' c = run . proc (head c) $ tail c

generateRandomString :: Int -> IO String
generateRandomString n = do
  take n . filter (\c -> c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'])) . randoms <$>
    newStdGen

getScreens :: MonadIO m => m [ScreenId]
getScreens =
  liftIO $ do
    screens <-
      do dpy <- openDisplay ""
         rects <- getScreenInfo dpy
         closeDisplay dpy
         return rects
    let ids = zip [0 ..] screens
    return $ map fst ids
