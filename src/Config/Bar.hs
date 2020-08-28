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
import Control.Monad (forM, forM_)
import Data.List (findIndex, isPrefixOf, partition, tails)
import Data.Maybe (isJust)
import System.Posix (ProcessID, forkProcess, killProcess, signalProcess)
import Xmobar (Runnable (..), xmobar)
import XMonad (ExtensionClass (..), X, XConfig (..), liftIO)
import qualified XMonad.Util.ExtensibleState as XS

class WMExec e where
  alias :: e -> String
  start :: e -> (String -> IO ()) -> IO ()

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

addIfExists :: WMRunnable -> [Xmobar] -> IO [Xmobar]
addIfExists (WMRun r) bars = do
  (chans, addedBars) <- addChans partitioned
  _ <- forkProcess $ start r $ \s -> forM_ chans (`writeChan` s)
  return addedBars
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
cleanupBars = (liftIO . mapM_ (signalProcess killProcess)) . pids =<< XS.get

startBars :: Bars -> X ()
startBars (runnables, bars) = startPids >>= XS.put . Bars
  where
    startPids :: X [ProcessID]
    startPids = do
      toStart <- liftIO $ addChans runnables bars
      forM toStart $ \c -> forkPID . xmobar $ xmobarToConfig c
    addChans :: [WMRunnable] -> [Xmobar] -> IO [Xmobar]
    addChans [] b = return b
    addChans (r:rs) b = do
      added <- addIfExists r b
      addChans rs added

statusBars :: Bars -> XConfig l -> XConfig l
statusBars bars c =
  c {startupHook = startupHook c >> liftIO cleanupPipes >> startBars bars}
