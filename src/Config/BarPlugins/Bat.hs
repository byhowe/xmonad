{-# LANGUAGE NamedFieldPuns #-}

module Config.BarPlugins.Bat
  ( Bat(..)
  , defaultFormatter
  ) where

import qualified Config.Bar as Bar (WMExec (..))
import Control.Monad (filterM, forM)
import qualified Data.ByteString.Char8 as B
import Data.Default (Default (..))
import Data.Traversable (for)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)

data Bat =
  Bat
    { alias     :: String
    , rate      :: Int
    , formatter :: [(String, Int)] -> String
    }

instance Default Bat where
  def = Bat {alias = "battery", formatter = defaultFormatter, rate = 300}

defaultFormatter :: [(String, Int)] -> String
defaultFormatter stats =
  concat $ for stats $ \(_, pcntg) -> batSymbol ++ "  " ++ show pcntg ++ "%"
  where
    batSymbol = "\62016"

instance Bar.WMExec Bat where
  alias Bat {alias} = alias
  rate Bat {rate} = rate
  iteration e@Bat {formatter} cb = do
    bats <- getAllBatteries
    pcntgs <-
      forM bats $ \bat -> do
        pcntg <- getPercentage bat
        return (bat, pcntg)
    cb $ formatter pcntgs
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
