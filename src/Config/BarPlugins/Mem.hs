{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.BarPlugins.Mem
  ( Mem(..)
  , defaultFormatter
  ) where

import qualified Config.Bar as Bar (WMExec (..))
import qualified Data.ByteString.Char8 as B
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import System.IO.Error (catchIOError)

data Mem =
  Mem
    { alias     :: String
    , formatter :: MemInfo -> String
    , rate      :: Int
    }

instance Default Mem where
  def = Mem {alias = "memory", formatter = defaultFormatter, rate = 10}

defaultFormatter :: MemInfo -> String
defaultFormatter MemInfo {total, available} =
  let stat = floor $ (fromIntegral $ total - available :: Float) / 1024.0 :: Int
   in show stat ++ "M"

instance Bar.WMExec Mem where
  alias Mem {alias} = alias
  rate Mem {rate} = rate
  iteration e@Mem {formatter} cb = do
    info <- getMemInfo
    cb . formatter $ info
    return e

data MemInfo =
  MemInfo
    { total     :: Int
    , free      :: Int
    , available :: Int
    }

memFile :: String
memFile = "/proc/meminfo"

getMemInfo :: IO MemInfo
getMemInfo = do
  flip catchIOError (const . return $ MemInfo 0 0 0) $ do
    info <- B.readFile memFile
    return $ getMemInfo' (lines . B.unpack $ info) Nothing Nothing Nothing
  where
    getMemInfo' :: [String] -> Maybe Int -> Maybe Int -> Maybe Int -> MemInfo
    getMemInfo' _ (Just t) (Just f) (Just a) =
      MemInfo {total = t, free = f, available = a}
    getMemInfo' [] t f a =
      MemInfo
        {total = fromMaybe 0 t, free = fromMaybe 0 f, available = fromMaybe 0 a}
    getMemInfo' (line:rest) t f a
      | startsWith line "MemTotal" = getMemInfo' rest (Just $ getInt line) f a
      | startsWith line "MemFree" = getMemInfo' rest t (Just $ getInt line) a
      | startsWith line "MemAvailable" =
        getMemInfo' rest t f (Just $ getInt line)
      | otherwise = getMemInfo' rest t f a
    startsWith :: String -> String -> Bool
    startsWith h n = take (length n) h == n
    getInt :: String -> Int
    getInt h = read $ getInt' h ""
      where
        getInt' :: String -> String -> String
        getInt' [] acc = acc
        getInt' (c:rest) acc
          | c `elem` ['0' .. '9'] = getInt' rest $ acc ++ [c]
          | not . null $ acc = acc
          | otherwise = getInt' rest ""
