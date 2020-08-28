{-# LANGUAGE ExistentialQuantification #-}

module Config.Bar
  ( statusBars
  , Bars
  , WMRunnable(..)
  , WMExec(..)
  , cleanupBars
  ) where

import Config.Chan (Chan (..), cleanupPipes, dupChan, newChan, writeChan)
import Config.Util (forkPID)
import Config.Xmobar (ChanReader (..), Xmobar (..), xmobarToConfig)
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_)
import Data.List (findIndex, isPrefixOf, partition, tails)
import Data.Maybe (isJust)
import Data.Monoid (All (All))
import Graphics.X11.Xlib.Extras
import System.IO.Error (catchIOError)
import System.Posix (ProcessID, forkProcess, killProcess, signalProcess)
import Xmobar (Runnable (..), xmobar)
import XMonad (ExtensionClass (..), X, XConfig (..), liftIO)
import qualified XMonad.Util.ExtensibleState as XS

class WMExec e where
  alias :: e -> String
  rate :: e -> Int
  rate _ = 10
  iteration :: e -> (String -> IO ()) -> IO e
  iteration e cb = cb "iteration not implemented" >> return e
  start :: e -> (String -> IO ()) -> IO ()
  start e cb = do
    newE <- iteration e cb
    threadDelay $ rate e * 100000
    start newE cb
  initialize :: e -> (String -> IO ()) -> X ()
  initialize _ _ = return ()

data WMRunnable =
  forall e. (WMExec e) =>
            WMRun e

instance WMExec WMRunnable where
  alias (WMRun a) = alias a
  start (WMRun a) = start a

type Bars = ([WMRunnable], [Xmobar])

newtype BarInfo =
  Bars
    { pids :: [ProcessID]
    }

instance ExtensionClass BarInfo where
  initialValue = Bars {pids = []}

addIfExists :: WMRunnable -> [Xmobar] -> X ([Xmobar], ProcessID)
addIfExists (WMRun r) bars = do
  (chans, addedBars) <- liftIO $ addChans partitioned
  let cb = \s -> forM_ chans (`writeChan` s)
  initialize r cb
  pid <- liftIO . forkProcess $ start r cb
  return (addedBars, pid)
  where
    addChans :: ([Xmobar], [Xmobar]) -> IO ([Chan], [Xmobar])
    addChans (toAddBars, b) = do
      c <-
        forM toAddBars $ \bar -> do
          chan <- newChan
          dup <- dupChan chan
          return (chan, addChan dup bar)
      let (chans, chanBars) = unzip c
      return (chans, chanBars ++ b)
    addChan :: Chan -> Xmobar -> Xmobar
    addChan chan bar =
      bar {commands = Run (ChanReader chan (alias r)) : commands bar}
    partitioned :: ([Xmobar], [Xmobar])
    partitioned =
      partition
        (\Xmobar {template = tmpl, sepChar = sep} ->
           isJust $ findIndex (isPrefixOf (sep ++ alias r ++ sep)) (tails tmpl))
        bars

cleanupBars :: X ()
cleanupBars =
  (liftIO .
   mapM_ (\p -> catchIOError (signalProcess killProcess p) $ \_ -> return ())) .
  pids =<<
  XS.get

statusBarsEventHook :: Event -> X All
statusBarsEventHook RRScreenChangeNotifyEvent {} =
  cleanupBars >> return (All True)
statusBarsEventHook _ = return $ All True

startBars :: X Bars -> X ()
startBars xBars = do
  (runnables, bars) <- xBars
  let startPids :: X [ProcessID]
      startPids = do
        (toStart, ps) <- addChans runnables bars []
        barPs <- forM toStart $ \c -> forkPID . xmobar $ xmobarToConfig c
        return $ barPs ++ ps
  startPids >>= XS.put . Bars
  where
    addChans ::
         [WMRunnable] -> [Xmobar] -> [ProcessID] -> X ([Xmobar], [ProcessID])
    addChans [] b ps = return (b, ps)
    addChans (r:rs) b ps = do
      (added, pid) <- addIfExists r b
      addChans rs added $ pid : ps

statusBars :: X Bars -> XConfig l -> XConfig l
statusBars bars c =
  c
    { startupHook = startupHook c >> liftIO cleanupPipes >> startBars bars
    , handleEventHook = statusBarsEventHook >> handleEventHook c
    }
