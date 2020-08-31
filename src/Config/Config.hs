{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Config.Config
  ( config
  ) where

import Config.Bar (Bars, WMRunnable (..), statusBars)
import qualified Config.BarPlugins.Bat as Bat (Bat (..))
import qualified Config.BarPlugins.Net as Net (Net (..))
import Config.BarPlugins.WMReader (wMReader)
import qualified Config.BarPlugins.WMReader as WMReader (WMReader (..))
import Config.ColorScheme (ColorScheme (..), challengerDeepCS)
import Config.Dmenu
    ( Dmenu (center, height, ignoreCase, lineCount, prompt)
    , dmenuDefaults
    , dmenuRun
    )
import Config.Font (Font (..))
import Config.Fullscreen (fullscreen)
import Config.Operations (recompileRestart, restart)
import Config.Scratchpad
    (Scratchpads, floatScratchpad, scratchpadTerminal, setupScratchpads)
import Config.Terminal (Terminal (..), alacritty, spawnTerminal)
import Config.Util (run')
import Config.WindowBringer (decorateName, gotoWindow)
import Config.Xmobar (barForeachScreen, foreachScreen)
import qualified Config.Xmobar as Bar (StaticReader (..), Xmobar (..))
import Data.Default (Default (..))
import qualified Data.Map as M
import Data.Monoid (All (..))
import Graphics.X11.ExtraTypes
import Graphics.X11.Xlib hiding (Font)
import Graphics.X11.Xrandr (xrrSelectInput)
import System.Exit (exitSuccess)
import System.Process (readProcess)
import Text.Printf (printf)
import Xmobar (Align (C), Border (BottomB), Runnable (Run), XPosition (TopSize))
import XMonad hiding (Default (..), Font, XConfig (..), config, restart)
import XMonad (XConfig)
import qualified XMonad (XConfig (..))
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Hooks.DynamicLog (PP (..), shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
    (ToggleStruts (..), avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog, isInProperty)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook (BorderUrgencyHook (..), withUrgencyHook)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Renamed (Rename (..), renamed)
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)

font :: Font
font = def {fontName = "JetBrainsMono Nerd Font", fontSize = 11}

cs :: ColorScheme
cs = challengerDeepCS

terminal :: Terminal
terminal = alacritty

tmux :: Terminal
tmux = terminal {cmd = Just ["tmux", "-u"]}

dmenu :: Dmenu
dmenu =
  (dmenuDefaults font cs)
    {ignoreCase = True, prompt = Just ">> ", height = Just barSize}

widgetSeperator :: String
widgetSeperator = xmobarColor (brightBg cs) "" " | "

bar :: Bar.Xmobar
bar =
  def
    { Bar.font = font
    , Bar.colorScheme = cs
    , Bar.alpha = 255
    , Bar.position = TopSize C 100 $ fromInteger barSize
    , Bar.border = BottomB
    , Bar.lowerOnStart = True
    , Bar.persistent = True
    , Bar.commands =
        [Run $ Bar.StaticReader (readProcess "uname" ["-r"] []) "kernel"]
    , Bar.template =
        concat
          [ "}"
          , xmobarColor (yellow cs) "" "mpd"
          , "{"
          , xmobarColor (red cs) "" "\61820 %kernel%"
          , widgetSeperator
          , xmobarColor (yellow cs) "" "%network%"
          , widgetSeperator
          , xmobarColor (blue cs) "" "\61463  cpu"
          , widgetSeperator
          , xmobarColor (magenta cs) "" "\61888  memory"
          , widgetSeperator
          , xmobarColor (yellow cs) "" "\61479  sound"
          , widgetSeperator
          , xmobarColor (red cs) "" "%battery%"
          , widgetSeperator
          , xmobarColor (green cs) "" "\61747  date"
          , " "
          ]
    }

focusedPP :: PP
focusedPP =
  namedScratchpadFilterOutWorkspacePP
    xmobarPP
      { ppCurrent = xmobarColor (blue cs) "" . wrap "[" "]"
      , ppVisible = xmobarColor (green cs) "" . wrap "(" ")"
      , ppHidden = xmobarColor (yellow cs) "" . wrap " " " "
      , ppUrgent = xmobarColor (red cs) "" . wrap "(" ")"
      , ppSep = widgetSeperator
      , ppTitle = xmobarColor (magenta cs) "" . shorten 40
      , ppLayout = xmobarColor (green cs) ""
      , ppExtras = [windowCount]
      , ppOrder =
          \(ws:l:t:ex) -> [ws, l] ++ map (xmobarColor (yellow cs) "") ex ++ [t]
      }
  where
    windowCount :: X (Maybe String)
    windowCount =
      gets $
      Just .
      show .
      length . W.integrate' . W.stack . W.workspace . W.current . windowset

normalPP :: PP
normalPP =
  focusedPP
    { ppCurrent = xmobarColor (blue cs) "" . wrap "[" "]"
    , ppVisible = xmobarColor (fg cs) "" . wrap "(" ")"
    , ppHidden = xmobarColor (fg cs) "" . wrap " " " "
    , ppTitle = xmobarColor (fg cs) "" . shorten 40
    }

browser :: [String]
browser = ["firefox-developer-edition"]

editor :: [String]
editor = ["emacs"]

workspaces :: [String]
workspaces = map show [1 .. 9 :: Int]

modm :: KeyMask
modm = mod4Mask

barSize :: Integer
barSize = 16

scratchpadMask :: KeyMask
scratchpadMask = modm .|. shiftMask

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys _ =
  M.fromList $
  -- applications
  [ ((modm, xK_Return), spawnTerminal tmux)
  , ((modm, xK_a), spawnTerminal terminal)
  , ((modm, xK_w), run' browser)
  , ((modm, xK_e), run' editor)
  -- menu
  , ((modm, xK_r), dmenuRun dmenu)
  , ( (modm .|. shiftMask, xK_w)
    , gotoWindow dmenu {center = True, lineCount = Just 12} $
      decorateName $ \n ws -> printf "%s : %s" ws n)
    -- navigation
  , ((modm .|. shiftMask, xK_q), kill)
  , ((modm .|. shiftMask, xK_Tab), nextScreen)
  , ((modm, xK_Tab), windows W.focusDown)
  , ((modm, xK_j), windows W.focusDown)
  , ((modm, xK_k), windows W.focusUp)
  , ((modm, xK_space), windows W.focusMaster)
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  , ((modm .|. shiftMask, xK_space), windows W.swapMaster)
  -- windows
  , ((modm, xK_t), withFocused $ windows . W.sink)
  , ((modm, xK_b), withFocused toggleBorder >> refresh)
  , ((modm, xK_p), sendMessage ToggleStruts)
  -- layouts
  , ((modm, xK_i), sendMessage NextLayout)
  , ((modm .|. controlMask, xK_j), sendMessage Shrink)
  , ((modm .|. controlMask, xK_k), sendMessage Expand)
  , ((modm .|. controlMask, xK_h), sendMessage $ IncMasterN 1)
  , ((modm .|. controlMask, xK_l), sendMessage $ IncMasterN $ -1)
  -- xmonad
  , ((modm .|. controlMask .|. shiftMask, xK_r), recompileRestart terminal)
  , ((modm .|. controlMask .|. shiftMask, xK_q), liftIO exitSuccess)
    -- multimedia
  , ((0, xF86XK_AudioMute), run' ["amixer", "set", "Master", "toggle"])
  , ( (0, xF86XK_AudioLowerVolume)
    , run' ["amixer", "set", "Master", "5%-", "unmute"])
  , ( (0, xF86XK_AudioRaiseVolume)
    , run' ["amixer", "set", "Master", "5%+", "unmute"])
  , ((modm, xF86XK_AudioLowerVolume), run' ["mpc", "volume", "-5"])
  , ((modm, xF86XK_AudioRaiseVolume), run' ["mpc", "volume", "+5"])
  , ((modm, xF86XK_AudioPrev), run' ["mpc", "seekthrough", "-00:00:05"])
  , ((modm, xF86XK_AudioNext), run' ["mpc", "seekthrough", "+00:00:05"])
  , ((0, xF86XK_AudioPrev), run' ["mpc", "prev"])
  , ((0, xF86XK_AudioNext), run' ["mpc", "next"])
  , ((0, xF86XK_AudioPlay), run' ["mpc", "toggle"])
  , ((0, xF86XK_AudioStop), run' ["mpc", "stop"])
  , ((modm, xK_Left), run' ["mpc", "seekthrough", "-00:00:05"])
  , ((modm, xK_Right), run' ["mpc", "seekthrough", "+00:00:05"])
  , ((modm, xK_Up), run' ["mpc", "next"])
  , ((modm, xK_Down), run' ["mpc", "prev"])
  , ((modm .|. controlMask, xK_Up), run' ["mpc", "toggle"])
  , ((modm .|. controlMask, xK_Down), run' ["mpc", "stop"])
  ] ++
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip workspaces [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

mouseBindings :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
mouseBindings _ =
  M.fromList
    [ ( (modm, button1)
      , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ( (modm, button3)
      , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

scratchpads :: Scratchpads
scratchpads =
  scratchpadTerminal
    [ (xK_a, tmux, centerFloat)
    , (xK_s, terminal {cmd = Just ["pulsemixer"]}, centerFloat)
    , (xK_d, terminal {cmd = Just ["ranger"]}, centerFloat)
    , (xK_m, terminal {cmd = Just ["ncmpcpp"]}, centerFloat)
    , (xK_p, terminal {cmd = Just ["htop"]}, centerFloat)
    ]
  where
    centerFloat = floatScratchpad 0.9 0.9 0.95 0.95

bars :: X Bars
bars = do
  foreachBars <-
    barForeachScreen
      (\(S s) ->
         bar {Bar.template = " %wmreader" ++ show s ++ "%" ++ Bar.template bar})
  foreachReaders <-
    foreachScreen
      (\s ->
         return $
         WMRun $
         (def :: WMReader.WMReader)
           { WMReader.alias = "wmreader"
           , WMReader.focusedPP = focusedPP
           , WMReader.normalPP = normalPP
           , WMReader.screenId = s
           })
  return
    ( WMRun (def :: Net.Net) {Net.rate = 5} :
      WMRun (def :: Bat.Bat) : foreachReaders
    , foreachBars)

logHook :: X ()
logHook = return ()

startupHook :: X ()
startupHook = do
  randrSetup
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr

manageHook :: ManageHook
manageHook = manageRules <+> manageDocks
  where
    manageRules =
      composeAll $
      [ isDialog --> doCenterFloat
      , windowRole "pop-up" --> doCenterFloat
      , windowType "SPLASH" --> doCenterFloat
      ] ++
      [ className =? x --> doCenterFloat
      | x <- ["vlc", "Xarchiver", "Bitwarden", "Sxiv", "tutanota-desktop"]
      ]
    windowRole = (stringProperty "WM_WINDOW_ROLE" =?)
    windowType t =
      isInProperty "_NET_WM_WINDOW_TYPE" $ "_NET_WM_WINDOW_TYPE_" ++ t

handleEventHook :: Event -> X All
handleEventHook RRScreenChangeNotifyEvent {} = do
  run' ["nitrogen", "--restore"]
  restart
  return $ All False
handleEventHook ClientMessageEvent {ev_message_type = mt} = do
  a <- getAtom "WINDOW_MANAGER_RESTART"
  if mt == a
    then do
      restart
      return $ All False
    else return $ All True
handleEventHook _ = return $ All True

randrSetup :: X ()
randrSetup = do
  dpy <- asks display
  root <- asks theRoot
  liftIO $ xrrSelectInput dpy root rrScreenChangeNotifyMask

config =
  wMReader $
  statusBars bars $
  withUrgencyHook (BorderUrgencyHook . red $ cs) $
  setupScratchpads scratchpads scratchpadMask $
  fullscreen $
  ewmh $
  docks $
  def
    { XMonad.borderWidth = 2
    , XMonad.workspaces = workspaces
    , XMonad.layoutHook = layoutHook
    , XMonad.terminal = command terminal
    , XMonad.normalBorderColor = bg cs
    , XMonad.focusedBorderColor = blue cs
    , XMonad.modMask = modm
    , XMonad.keys = keys
    , XMonad.logHook = logHook
    , XMonad.startupHook = startupHook
    , XMonad.mouseBindings = mouseBindings
    , XMonad.manageHook = manageHook
    , XMonad.handleEventHook = handleEventHook
    , XMonad.focusFollowsMouse = True
    , XMonad.clickJustFocuses = True
    , XMonad.handleExtraArgs = \_ c -> return c
    }
  where
    layoutHook =
      avoidStruts $
      tallLayout ||| mirrorTallLayout ||| monocleLayout ||| floatingLayout
      where
        tallLayout =
          renamed [Replace "tall"] $
          leaveSpace 3 $ ResizableTall nmaster delta ratio []
        mirrorTallLayout = renamed [Replace "mirror tall"] $ Mirror tallLayout
        monocleLayout = renamed [Replace "monocle"] $ noBorders Full
        floatingLayout = renamed [Replace "float"] simplestFloat
        nmaster = 1
        ratio = 1 / 2
        delta = 3 / 100
        leaveSpace i =
          spacingRaw False (Border i i i i) True (Border i i i i) True
