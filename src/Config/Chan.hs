{-# LANGUAGE NamedFieldPuns #-}

module Config.Chan
  ( Chan(..)
  , getChan
  , newChan
  , readChan
  , writeChan
  , dupChan
  , cleanupPipes
  ) where

import Config.Util (generateRandomString)
import Control.Monad (forM_)
import Data.Bits ((.|.))
import System.Directory
    (doesDirectoryExist, getTemporaryDirectory, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO
    (Handle, IOMode (ReadWriteMode), hFlush, hGetLine, hPutStrLn, openFile)
import System.Posix (createNamedPipe, ownerReadMode, ownerWriteMode)

data Chan =
  Chan
    { path :: FilePath
    , h    :: Handle
    }

pipePrefix :: String
pipePrefix = "xmonad-chan-"

generateRandomPath :: IO FilePath
generateRandomPath = do
  a <- genPath
  e <- doesDirectoryExist =<< genPath
  if e
    then generateRandomPath
    else return a
  where
    genPath :: IO FilePath
    genPath = do
      temp <- getTemporaryDirectory
      s <- generateRandomString 12
      return $ temp </> pipePrefix ++ s

getChan :: FilePath -> IO Chan
getChan p = do
  handle <- openFile p ReadWriteMode
  return $ Chan {path = p, h = handle}

dupChan :: Chan -> IO Chan
dupChan Chan {path} = getChan path

newChan :: IO Chan
newChan = do
  p <- generateRandomPath
  createNamedPipe p (ownerReadMode .|. ownerWriteMode)
  getChan p

readChan :: Chan -> IO String
readChan Chan {h} = hGetLine h

writeChan :: Chan -> String -> IO ()
writeChan Chan {h} s = hPutStrLn h s >> hFlush h

cleanupPipes :: IO ()
cleanupPipes = do
  temp <- getTemporaryDirectory
  dirs <- listDirectory temp
  let toDelete =
        filter (\dir -> take (length pipePrefix) dir == pipePrefix) dirs
  forM_ (map (temp </>) toDelete) removeFile
