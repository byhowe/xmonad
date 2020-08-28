module Config.BarPlugins
  ( Interface(..)
  , Net(..)
  ) where

import Config.Bar (WMExec (..))
import Control.Monad (filterM)
import System.Directory (listDirectory)

data Interface
  = Exclude [String]
  | Include [String]

data Net =
  Net String Interface

instance WMExec Net where
  alias (Net a _) = a
  start (Net _ ifs) cb = getInterfaces ifs >>= cb . show

netDir :: String
netDir = "/sys/class/net"

getAllInterfaces :: IO [String]
getAllInterfaces = listDirectory netDir

getInterfaces :: Interface -> IO [String]
getInterfaces (Exclude ifs) =
  getAllInterfaces >>= filterM (\i -> return $ i `notElem` ifs)
getInterfaces (Include ifs) =
  getAllInterfaces >>= filterM (\i -> return $ i `elem` ifs)
