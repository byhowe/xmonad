{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.BarPlugins.Net
  ( Interface(..)
  , Transmission(..)
  , Net(..)
  , defaultFormatter
  ) where

import qualified Config.Bar as Bar (WMExec (..))
import Control.Monad (filterM, forM)
import qualified Data.ByteString.Char8 as B
import Data.Default (Default (..))
import qualified Data.Map as M
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)

data Interface
  = Exclude [String]
  | Include [String]

data Transmission
  = Rx
  | Tx

type Rx = Int

type Tx = Int

newtype Kilobytes =
  K Int
  deriving (Eq)

instance Show Kilobytes where
  show (K size) = show size ++ "K"

data Net =
  Net
    { alias     :: String
    , formatter :: M.Map String (Rx, Tx) -> String
    , rate      :: Int
    , interface :: Interface
    , previous  :: M.Map String (Rx, Tx)
    }

instance Default Net where
  def =
    Net
      { alias = "network"
      , formatter = defaultFormatter
      , rate = 10
      , interface = Exclude ["lo"]
      , previous = M.empty
      }

combineInterfaces :: M.Map String (Rx, Tx) -> (Rx, Tx)
combineInterfaces stats =
  foldl (\(accRx, accTx) (rx, tx) -> (accRx + rx, accTx + tx)) (0, 0) $
  M.elems stats

defaultFormatter :: M.Map String (Rx, Tx) -> String
defaultFormatter m =
  if M.size m == 0
    then "unavailable"
    else let (rx, tx) = combineInterfaces m
             rxK = show $ kilobytes rx
             txK = show $ kilobytes tx
             rxS = replicate (5 - length rxK) ' ' ++ rxK
             txS = txK ++ replicate (5 - length txK) ' '
          in rxS ++ "\61671" ++ txS

instance Bar.WMExec Net where
  alias Net {alias} = alias
  rate Net {rate} = rate
  iteration e@Net {formatter, rate, interface, previous} cb = do
    interfaces <- filtered
    bytes <-
      forM interfaces $ \i -> do
        (curRx, curTx) <- transmissionStats i
        case M.lookup i previous of
          Nothing -> return (i, (curRx, curTx), Nothing)
          Just (rx, tx) ->
            if (curRx < rx) || (curTx < tx)
              then return (i, (curRx, curTx), Nothing)
              else return
                     ( i
                     , (curRx, curTx)
                     , Just
                         ( floor $
                           (10.0 / (fromIntegral rate :: Float)) *
                           (fromIntegral $ curRx - rx :: Float)
                         , floor $
                           (10.0 / (fromIntegral rate :: Float)) *
                           (fromIntegral $ curTx - tx :: Float)))
    cb . formatter . M.fromList $
      filterMap
        (\(i, _, diff) ->
           case diff of
             Nothing -> Nothing
             Just d  -> Just (i, d))
        bytes
    return $ e {previous = M.fromList $ map (\(i, cur, _) -> (i, cur)) bytes}
    where
      filtered :: IO [String]
      filtered = filterM isUsable =<< getInterfaces interface
      transmissionStats :: String -> IO (Rx, Tx)
      transmissionStats i = do
        rx <- transmissionBytes Rx i
        tx <- transmissionBytes Tx i
        return (rx, tx)
      filterMap :: (a -> Maybe b) -> [a] -> [b]
      filterMap _ [] = []
      filterMap p (a:as) =
        case p a of
          Nothing -> filterMap p as
          Just b  -> b : filterMap p as

netDir :: String
netDir = "/sys/class/net"

getAllInterfaces :: IO [String]
getAllInterfaces = listDirectory netDir

getInterfaces :: Interface -> IO [String]
getInterfaces (Exclude ifs) =
  getAllInterfaces >>= filterM (\i -> return $ i `notElem` ifs)
getInterfaces (Include ifs) =
  getAllInterfaces >>= filterM (\i -> return $ i `elem` ifs)

isUsable :: String -> IO Bool
isUsable i =
  flip catchIOError (const $ return False) $ do
    operstate <- B.readFile $ netDir </> i </> "operstate"
    return $! B.init operstate `elem` ["up", "unknown"]

transmissionBytes :: Transmission -> String -> IO Int
transmissionBytes t i =
  let d =
        case t of
          Rx -> "rx_bytes"
          Tx -> "tx_bytes"
   in flip catchIOError (const $ return 0) $ do
        bytes <- B.readFile $ netDir </> i </> "statistics" </> d
        let Just (rawBytes, _) = B.readInt . B.init $ bytes
        return $! rawBytes

kilobytes :: Int -> Kilobytes
kilobytes n = K . floor $ (fromIntegral n :: Float) / 1024.0
