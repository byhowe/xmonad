module Config.Keybindings (applications) where

import qualified Data.Map as M
import XMonad (XConfig (keys), spawn)
import XMonad.Util.EZConfig (mkKeymap)

applications :: XConfig l -> XConfig l
applications conf = conf { keys = M.union appKeys . keys conf }
  where
    appKeys = mkKeymap conf
      [ ("M-a", spawn "alacritty")
      , ("M-w", spawn "firefox-developer-edition -P 'arkenfox-vpn' -new-window")
      , ("M-o", spawn "mullvad-exclude firefox-developer-edition -P 'arkenfox-clear' -new-window")
      , ("M-e", spawn "emacs")
      , ("M-d", spawn "pcmanfm")
      , ("M-r", spawn "rofi -show run")
      , ("M-p", spawn "rofi -show drun")
      , ("M-v", spawn "rofi -show window") ]
