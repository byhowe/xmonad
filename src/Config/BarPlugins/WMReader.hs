{-# LANGUAGE NamedFieldPuns #-}

module Config.BarPlugins.WMReader
  ( wMReaderLog
  , wMReaderLog'
  , wMReader
  , WMReader(..)
  ) where

import qualified Config.Bar as Bar (WMExec (..))
import Control.Monad (forM_)
import Data.Default.Class (Default (..))
import XMonad hiding (Default (..))
import XMonad.Hooks.DynamicLog (PP, dynamicLogString, xmobarPP)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

data WMReader =
  WMReader
    { alias      :: String
    , focusedPP  :: PP
    , normalPP   :: PP
    , screenId   :: ScreenId
    , dynamicLog :: PP -> X String
    }

instance Default WMReader where
  def =
    WMReader
      { alias = "wmreader"
      , focusedPP = xmobarPP
      , normalPP = xmobarPP
      , screenId = S 0
      , dynamicLog = dynamicLogString
      }

instance Bar.WMExec WMReader where
  alias WMReader {alias, screenId} =
    let (S s) = screenId
     in alias ++ show s
  start _ _ = return ()
  initialize WMReader {focusedPP, normalPP, screenId, dynamicLog} cb =
    info >>= XS.put . WMReaderInfo
    where
      info :: X [(PP, PP, ScreenId, PP -> X String, String -> IO ())]
      info = do
        prev <- sbInfo <$> XS.get
        return $ (focusedPP, normalPP, screenId, dynamicLog, cb) : prev

newtype WMReaderInfo =
  WMReaderInfo
    { sbInfo :: [(PP, PP, ScreenId, PP -> X String, String -> IO ())]
    }

instance ExtensionClass WMReaderInfo where
  initialValue = WMReaderInfo []

wMReaderLog :: X ()
wMReaderLog = do
  sb <- sbInfo <$> XS.get
  wMReaderLog' sb

wMReaderLog' :: [(PP, PP, ScreenId, PP -> X String, String -> IO ())] -> X ()
wMReaderLog' bars = do
  st <- get
  forM_ bars $ \(fPP, nPP, s, dynlStr, cb) -> do
    ws <- screenWorkspace s
    case ws of
      Nothing -> return ()
      Just w -> do
        out <-
          dynlStr $
          if (w ==) . W.tag . W.workspace . W.current $ windowset st
            then fPP
            else nPP
        liftIO $ cb out

wMReader :: XConfig l -> XConfig l
wMReader c = c {logHook = logHook c >> wMReaderLog}
