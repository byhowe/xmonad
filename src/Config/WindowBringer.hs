{-# LANGUAGE TupleSections #-}

module Config.WindowBringer
  ( gotoWindow
  , bringerAction
  , windowMap
  , decorateName
  , Titler
  ) where

import Config.Dmenu
import qualified Data.Map as M
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows (getName)

type Titler = WindowSpace -> Window -> X String

gotoWindow :: Dmenu -> Titler -> X ()
gotoWindow c titler = bringerAction c titler W.focusWindow

bringerAction :: Dmenu -> Titler -> (Window -> WindowSet -> WindowSet) -> X ()
bringerAction c titler action =
  windowMap titler >>= runDmenu >>= flip whenJust (windows . action)
  where
    runDmenu :: M.Map String Window -> X (Maybe Window)
    runDmenu = dmenuMap c

windowMap :: Titler -> X (M.Map String Window)
windowMap titler = do
  ws <- gets windowset
  M.fromList . concat <$> mapM keyValuePairs (W.workspaces ws)
  where
    keyValuePairs ws = mapM (keyValuePair ws) $ W.integrate' (W.stack ws)
    keyValuePair ws w = (, w) <$> titler ws w

decorateName :: (String -> String -> String) -> Titler
decorateName titler ws w = do
  name <- show <$> getName w
  return $ titler name (W.tag ws)
