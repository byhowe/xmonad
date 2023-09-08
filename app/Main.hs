module Main (main) where

import Config.Config (config)
import Config.Operations (recompile)
import Control.Monad (when)
import Data.Version (showVersion)
import Graphics.X11.Xinerama (compiledWithXinerama)
import Paths_xmonad_config (version)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.Info (arch, compilerName, compilerVersion, os)
import Text.Printf (printf)
import XMonad (Directories' (..), getDirectories, launch)

main :: IO ()
main = getArgs >>= cli

cli :: [String] -> IO ()
cli [help] | help `elem` ["help", "-h", "--help"] = printUsage >> exitSuccess
cli [ver] | ver `elem` ["version", "-v", "--version"] = printVersion >> exitSuccess
cli [info] | info `elem` ["info", "-vv", "--info"] = printInfo >> exitSuccess
cli ["dirs"] = printDirectories >> exitSuccess
cli ["recompile"] = recompile Nothing >>= \success -> when success exitSuccess >> exitFailure
cli ["run"] = getDirectories >>= launch config
cli a = do
  putStrLn . printf "Unrecognized command(s): %s\n" $ unwords a
  printUsage
  exitFailure

printUsage :: IO ()
printUsage = do
  self <- getProgName
  putStr $ printf
      "Usage: %s [COMMAND]\n\
      \Commands:\n\
      \  help       Print this message\n\
      \  version    Print the version number\n\
      \  info       Print the verbose version information\n\
      \  dirs       Print the directory information\n\
      \  recompile  Recompile xmonad\n\
      \  run        Run xmonad\n"
      self

printVersion :: IO ()
printVersion = putStrLn . printf "xmonad %s" $ showVersion version

printInfo :: IO ()
printInfo =
  putStr $
    printf
      "xmonad (byron howe) %s compiled by %s %s for %s-%s\n\
      \Xinerama: %s\n"
      (showVersion version)
      compilerName
      (showVersion compilerVersion)
      arch
      os
      (show compiledWithXinerama)

printDirectories :: IO ()
printDirectories = do
  dirs <- getDirectories
  putStr $
    printf
      "Config Dir: %s\n\
      \Data Dir:   %s\n\
      \Cache Dir:  %s\n"
      (cfgDir dirs)
      (dataDir dirs)
      (cacheDir dirs)
