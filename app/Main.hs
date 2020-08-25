module Main where

import Config.Config (config)
import Config.Operations (recompile, sendReplace, sendRestart)
import Config.Util (getConfigDir)
import Control.Monad (unless)
import Data.Version (showVersion)
import Graphics.X11.Xinerama (compiledWithXinerama)
import Paths_xmonad_config (version)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.Info (arch, compilerName, compilerVersion, os)
import Text.Printf (printf)
import XMonad (launch)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printUsage
    ["-h"] -> printUsage
    ["--version"] -> printVersion
    ["-v"] -> printVersion
    ["-vv"] -> printInfo
    ["--src-dir"] -> printConfigDir
    ["-s"] -> printConfigDir
    ["--recompile"] -> runRecompile
    ["-r"] -> runRecompile
    ["--restart"] -> runRestart
    ["-e"] -> runRestart
    ["--replace"] -> runReplace
    ["-p"] -> runReplace
    [] -> runXMonad
    a -> do
      putStrLn . printf "Unrecognized options: %s\n" $ unwords a
      printUsage
      exitFailure

printUsage :: IO ()
printUsage = do
  self <- getProgName
  putStrLn $
    printf
      "XMonad %s\n\
      \A small but functional ICCCM-compliant tiling window manager\n\
      \\n\
      \USAGE:\n\
      \    %s [FLAGS]\n\
      \\n\
      \FLAGS:\n\
      \    -h, --help        Prints help information\n\
      \    -v, --version     Prints version information\n\
      \    -s, --src-dir     Prints the source code path\n\
      \    -r, --recompile   Recompiles the source code\n\
      \    -e, --restart     Requests a running XMonad process to restart\n\
      \    -p, --replace     Replaces the currently running window manage"
      (showVersion version)
      self

printVersion :: IO ()
printVersion = putStrLn . printf "xmonad: %s" $ showVersion version

printInfo :: IO ()
printInfo = do
  configPath <- getConfigDir
  putStrLn $
    printf
      "XMonad %s compiled by %s %s for %s-%s\n\
      \Xinerama: %s\n\
      \Config path: %s"
      (showVersion version)
      compilerName
      (showVersion compilerVersion)
      arch
      os
      (show compiledWithXinerama)
      configPath

printConfigDir :: IO ()
printConfigDir = putStrLn =<< getConfigDir

runRecompile :: IO ()
runRecompile = recompile Nothing >>= flip unless exitFailure

runRestart :: IO ()
runRestart = sendRestart

runReplace :: IO ()
runReplace = sendReplace

runXMonad :: IO ()
runXMonad = launch config
