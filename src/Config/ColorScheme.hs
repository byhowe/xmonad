module Config.ColorScheme
  ( ColorScheme(..)
  , snazzyCS
  ) where

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
