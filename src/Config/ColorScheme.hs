module Config.ColorScheme
  ( ColorScheme(..)
  , snazzyCS
  , draculaCS
  ) where

import Data.Default (Default (..))

data ColorScheme =
  CS
    { base00 :: String
    , base01 :: String
    , base02 :: String
    , base03 :: String
    , base04 :: String
    , base05 :: String
    , base06 :: String
    , base07 :: String
    , base08 :: String
    , base09 :: String
    , base0A :: String
    , base0B :: String
    , base0C :: String
    , base0D :: String
    , base0E :: String
    , base0F :: String
    }

instance Default ColorScheme where
  def = snazzyCS

snazzyCS :: ColorScheme
snazzyCS =
  CS
    { base00 = "#282A36"
    , base01 = "#34353E"
    , base02 = "#43454F"
    , base03 = "#78787E"
    , base04 = "#A5A5A9"
    , base05 = "#E2E4E5"
    , base06 = "#EFF0EB"
    , base07 = "#F1F1F0"
    , base08 = "#FF5C57"
    , base09 = "#FF9F43"
    , base0A = "#F3F99D"
    , base0B = "#5AF78E"
    , base0C = "#9AEDFE"
    , base0D = "#57C7FF"
    , base0E = "#FF6AC1"
    , base0F = "#B2643C"
    }

draculaCS :: ColorScheme
draculaCS =
  CS
    { base00 = "#282936"
    , base01 = "#3A3C4E"
    , base02 = "#4D4F68"
    , base03 = "#626483"
    , base04 = "#62D6E8"
    , base05 = "#E9E9F4"
    , base06 = "#F1F2F8"
    , base07 = "#F7F7FB"
    , base08 = "#EA51B2"
    , base09 = "#B45BCF"
    , base0A = "#00F769"
    , base0B = "#EBFF87"
    , base0C = "#A1EFE4"
    , base0D = "#62D6E8"
    , base0E = "#B45BCF"
    , base0F = "#00F769"
    }
