module Config.Xmobar
  ( Xmobar(..)
  , ChanReader(..)
  , StaticReader(..)
  , xmobarToConfig
  , screen
  , foreachScreen
  , barForeachScreen
  ) where

import Config.Chan (Chan (..), readChan)
import Config.ColorScheme (ColorScheme (..))
import Config.Font (Font, getXft)
import Config.Util (getScreens)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default (..))
import Xmobar hiding (Config (..))
import qualified Xmobar as Bar (Config (..))
import XMonad (ScreenId (..), X)

data Xmobar =
  Xmobar
    { font             :: Font
    , colorScheme      :: ColorScheme
    , additionalFonts  :: [Font]
    , wmClass          :: String
    , wmName           :: String
    , position         :: XPosition
    , textOffset       :: Int
    , textOffsets      :: [Int]
    , iconOffset       :: Int
    , border           :: Border
    , borderWidth      :: Int
    , alpha            :: Int
    , hideOnStart      :: Bool
    , allDesktops      :: Bool
    , overrideRedirect :: Bool
    , pickBroadest     :: Bool
    , lowerOnStart     :: Bool
    , persistent       :: Bool
    , iconRoot         :: FilePath
    , commands         :: [Runnable]
    , sepChar          :: String
    , alignSep         :: String
    , template         :: String
    , verbose          :: Bool
    }

instance Default Xmobar where
  def =
    Xmobar
      { font = def
      , colorScheme = def
      , additionalFonts = []
      , wmClass = "xmobar"
      , wmName = "xmobar"
      , position = Top
      , textOffset = -1
      , textOffsets = []
      , iconOffset = -1
      , border = NoBorder
      , borderWidth = 1
      , alpha = 255
      , hideOnStart = False
      , allDesktops = True
      , overrideRedirect = True
      , pickBroadest = False
      , lowerOnStart = False
      , persistent = False
      , iconRoot = "."
      , commands = []
      , sepChar = "%"
      , alignSep = "}{"
      , template = ""
      , verbose = False
      }

xmobarToConfig :: Xmobar -> Bar.Config
xmobarToConfig c =
  Bar.Config
    { Bar.font = getXft $ font c
    , Bar.additionalFonts = map getXft $ additionalFonts c
    , Bar.wmClass = wmClass c
    , Bar.wmName = wmName c
    , Bar.bgColor = base01 $ colorScheme c
    , Bar.fgColor = base05 $ colorScheme c
    , Bar.alpha = alpha c
    , Bar.position = position c
    , Bar.border = border c
    , Bar.borderColor = base03 $ colorScheme c
    , Bar.borderWidth = borderWidth c
    , Bar.textOffset = textOffset c
    , Bar.iconOffset = iconOffset c
    , Bar.textOffsets = textOffsets c
    , Bar.hideOnStart = hideOnStart c
    , Bar.lowerOnStart = lowerOnStart c
    , Bar.persistent = persistent c
    , Bar.allDesktops = allDesktops c
    , Bar.overrideRedirect = overrideRedirect c
    , Bar.pickBroadest = pickBroadest c
    , Bar.iconRoot = iconRoot c
    , Bar.commands = commands c
    , Bar.sepChar = sepChar c
    , Bar.alignSep = alignSep c
    , Bar.template = template c
    , Bar.verbose = verbose c
    }

data ChanReader =
  ChanReader Chan String

instance Show ChanReader where
  show = undefined

instance Read ChanReader where
  readsPrec = undefined

instance Exec ChanReader where
  alias (ChanReader _ a) = a
  start (ChanReader chan _) cb = forever $ readChan chan >>= cb

data StaticReader =
  StaticReader (IO String) String

instance Show StaticReader where
  show = undefined

instance Read StaticReader where
  readsPrec = undefined

instance Exec StaticReader where
  alias (StaticReader _ a) = a
  start (StaticReader m _) cb = cb . head . lines =<< m

foreachScreen :: MonadIO m => (ScreenId -> m a) -> m [a]
foreachScreen m = mapM m =<< getScreens

barForeachScreen :: (ScreenId -> Xmobar) -> X [Xmobar]
barForeachScreen m =
  foreachScreen
    (\s ->
       let c = m s
        in return $ c {position = screen s c})

screen :: ScreenId -> Xmobar -> XPosition
screen (S s) c = OnScreen s $ position c
