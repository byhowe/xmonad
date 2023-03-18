module Config.ColorScheme
  ( ColorScheme(..)
  , challengerDeepCS
  ) where

import Data.Default.Class (Default (..))

data ColorScheme =
  CS
    { bg       :: String
    , brightBg :: String
    , fg       :: String
    , brightFg :: String
    , grey     :: String
    , blue     :: String
    , green    :: String
    , magenta  :: String
    , red      :: String
    , yellow   :: String
    }

instance Default ColorScheme where
  def = challengerDeepCS

challengerDeepCS :: ColorScheme
challengerDeepCS =
  CS
    { bg = "#12111E"
    , brightBg = "#1E1C31"
    , fg = "#B2B2B2"
    , brightFg = "#CBE3E7"
    , grey = "#6c71c4"
    , blue = "#63F2F1"
    , green = "#95FFA4"
    , magenta = "#C991E1"
    , red = "#FF8080"
    , yellow = "#FFE9AA"
    }
