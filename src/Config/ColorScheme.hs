module Config.ColorScheme
  ( ColorScheme(..)
  , challengerDeep
  ) where

import Data.Default (Default (..))

data ColorScheme =
  CS
    { bg1           :: String
    , bg2           :: String
    , fg1           :: String
    , fg2           :: String
    , red           :: String
    , brightRed     :: String
    , green         :: String
    , brightGreen   :: String
    , yellow        :: String
    , brightBlue    :: String
    , blue          :: String
    , magenta       :: String
    , brightMagenta :: String
    , brightCyan    :: String
    , cyan          :: String
    }

instance Default ColorScheme where
  def = challengerDeep

challengerDeep :: ColorScheme
challengerDeep =
  CS
    { bg1 = "#1E1C31"
    , bg2 = "#12111E"
    , fg1 = "#B2B2B2"
    , fg2 = "#CBE3E7"
    , red = "#FF8080"
    , brightRed = "#FFB378"
    , green = "#95FFA4"
    , brightGreen = "#63F2F1"
    , yellow = "#FFE9AA"
    , brightBlue = "#91DDFF"
    , blue = "#65B2FF"
    , magenta = "#C991E1"
    , brightMagenta = "#906CFF"
    , brightCyan = "#AAFFE4"
    , cyan = "#62D196"
    }
