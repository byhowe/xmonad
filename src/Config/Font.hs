module Config.Font
  ( Font(..)
  , getXft
  ) where

import Data.Default.Class (Default (..))
import Data.List (intercalate)

data Font =
  Font
    { fontName      :: String
    , fontSize      :: Integer
    , fontWeight    :: String
    , fontAntiAlias :: Bool
    , fontHinting   :: Bool
    }

instance Default Font where
  def =
    Font
      { fontName = "monospace"
      , fontSize = 12
      , fontWeight = "regular"
      , fontAntiAlias = True
      , fontHinting = True
      }

getXft :: Font -> String
getXft f =
  intercalate
    ":"
    [ "xft"
    , fontName f
    , "family=" ++ fontName f
    , "pixelsize=" ++ show (fontSize f)
    , "weight=" ++ fontWeight f
    , "antialias=" ++ showBool (fontAntiAlias f)
    , "hinting=" ++ showBool (fontHinting f)
    ]
  where
    showBool :: Bool -> String
    showBool b =
      if b
        then "true"
        else "false"
