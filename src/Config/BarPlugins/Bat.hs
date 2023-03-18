{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.BarPlugins.Bat
  ( Bat(..)
  , defaultFormatter
  ) where

import qualified Config.Bar as Bar (WMExec (..))
import Control.Monad (filterM, forM)
import qualified Data.ByteString.Char8 as B
import Data.Default.Class (Default (..))
import Data.Traversable (for)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)

data Bat =
  Bat
    { alias     :: String
    , rate      :: Int
    , formatter :: [(String, Int, BatStatus)] -> String
    }

data BatStatus
  = Charging
  | Discharging
  | Full
  deriving (Eq)

instance Default Bat where
  def = Bat {alias = "battery", formatter = defaultFormatter, rate = 50}

defaultFormatter :: [(String, Int, BatStatus)] -> String
defaultFormatter stats =
  concat $
  for stats $ \(_, pcntg, status) ->
    let pcntg' = show pcntg
     in symbol pcntg status ++
        "  " ++ replicate (3 - length pcntg') ' ' ++ pcntg' ++ "%"
  where
    symbol pcntg status
      | status /= Discharging = plugSymbol
      | pcntg > 90 = batFull
      | pcntg > 65 = batThreeQuarters
      | pcntg > 45 = batHalf
      | pcntg > 15 = batQuarter
      | otherwise = batEmpty
    batEmpty = "\62020"
    batFull = "\62016"
    batThreeQuarters = "\62017"
    batHalf = "\62018"
    batQuarter = "\62019"
    plugSymbol = "\61926"

instance Bar.WMExec Bat where
  alias Bat {alias} = alias
  rate Bat {rate} = rate
  iteration e@Bat {formatter} cb = do
    bats <- getAllBatteries
    stats <-
      forM bats $ \bat -> do
        pcntg <- getPercentage bat
        status <- getStatus bat
        return (bat, pcntg, status)
    cb $ formatter stats
    return e

batDir :: String
batDir = "/sys/class/power_supply"

getAllBatteries :: IO [String]
getAllBatteries =
  filterM (\s -> return $ take 3 s == "BAT") =<< listDirectory batDir

getPercentage :: String -> IO Int
getPercentage bat = do
  flip catchIOError (const $ return 0) $ do
    bytes <- B.readFile $ batDir </> bat </> "capacity"
    let Just (percentage, _) = B.readInt . B.init $ bytes
    return $! percentage

getStatus :: String -> IO BatStatus
getStatus bat =
  flip catchIOError (const $ return Discharging) $ do
    status <- B.readFile $ batDir </> bat </> "status"
    return $
      case B.init status of
        "Charging"    -> Charging
        "Discharging" -> Discharging
        "Full"        -> Full
        _             -> Discharging
