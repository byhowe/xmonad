module Config.Scratchpad
  ( Scratchpads
  , scratchpadTerminal
  , setupScratchpads
  , floatScratchpad
  ) where

import Config.Terminal
import qualified Data.Map as M
import Graphics.X11.Xlib
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.NamedScratchpad as NS

type Scratchpads = [(KeySym, X (), NS.NamedScratchpad)]

scratchpadTerminal :: [(KeySym, Terminal, ManageHook)] -> Scratchpads
scratchpadTerminal sc =
  [ ( key
    , spawnTerminal t {cls = Just $ "NS" ++ [i]}
    , NS.NS
        [i]
        (shellCommand t {cls = Just $ "NS" ++ [i]})
        (resource =? ("NS" ++ [i]))
        hook')
  | (i, (key, t, hook')) <- zip ['a' ..] sc
  ]

scratchpadBindings :: Scratchpads -> KeyMask -> M.Map (KeyMask, KeySym) (X ())
scratchpadBindings sc m =
  M.fromList
    [ ( (m .|. shiftMask, key)
      , NS.customRunNamedScratchpadAction (const action) (map third sc) $
        NS.name nsc)
    | (key, action, nsc) <- sc
    ]

setupScratchpads :: Scratchpads -> KeyMask -> XConfig a -> XConfig a
setupScratchpads sc m c =
  c
    { XMonad.keys = \cc -> M.union (XMonad.keys c cc) (scratchpadBindings sc m)
    , XMonad.manageHook =
        XMonad.manageHook c <+> NS.namedScratchpadManageHook (map third sc)
    }

floatScratchpad :: Rational -> Rational -> Rational -> Rational -> ManageHook
floatScratchpad h w t l = NS.customFloating $ W.RationalRect (l - w) (t - h) w h

third :: (a, b, c) -> c
third (_, _, c) = c
