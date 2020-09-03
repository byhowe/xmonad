{-# LANGUAGE NamedFieldPuns #-}

module Config.BarPlugins.Cpu
  ( Cpu(..)
  , defaultFormatter
  ) where

import qualified Config.Bar as Bar (WMExec (..))
import qualified Data.ByteString.Char8 as B
import Data.Default (Default (..))
import System.IO.Error (catchIOError)

data Cpu =
  Cpu
    { alias     :: String
    , formatter :: Float -> String
    , rate      :: Int
    , previous  :: (Float, Float)
    }

instance Default Cpu where
  def =
    Cpu
      { alias = "cpu"
      , formatter = defaultFormatter
      , rate = 10
      , previous = (0, 0)
      }

defaultFormatter :: Float -> String
defaultFormatter prcntg =
  let s = show (floor prcntg :: Int)
   in replicate (3 - length s) ' ' ++ s ++ "%"

instance Bar.WMExec Cpu where
  alias Cpu {alias} = alias
  rate Cpu {rate} = rate
  iteration e@Cpu {formatter, previous} cb = do
    stat <- getCpuInfo
    let tot = sum stat
        idle = stat !! 3
        (lastIdle, lastTot) = previous
        idleDelta = idle - lastIdle
        totDelta = tot - lastTot
        util = 100.0 * (1.0 - idleDelta / totDelta)
    cb . formatter $ util
    return $ e {previous = (idle, tot)}

cpuFile :: FilePath
cpuFile = "/proc/stat"

getCpuInfo :: IO [Float]
getCpuInfo =
  flip catchIOError (const . return $ replicate 10 0.0) $
  cpuData <$> B.readFile cpuFile
  where
    readInt :: B.ByteString -> Int
    readInt bs =
      case B.readInt bs of
        Nothing     -> 0
        Just (i, _) -> i
    cpuData :: B.ByteString -> [Float]
    cpuData = map (fromIntegral . readInt) . tail . B.words . head . B.lines
