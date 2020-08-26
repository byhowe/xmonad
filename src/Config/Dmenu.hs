{-# LANGUAGE LambdaCase #-}

module Config.Dmenu
  ( Dmenu(..)
  , dmenuDefaults
  , dmenuDefaults'
  , dmenu
  , dmenuRun
  , dmenuMap
  ) where

import Config.ColorScheme (ColorScheme (..))
import Config.Font (Font, getXft)
import Config.Util (fork)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import System.Process
import XMonad.Util.Run (runProcessWithInput)

data Dmenu =
  Dmenu
    -- behaviour
    { font               :: Maybe Font
    , bottom             :: Bool
    , center             :: Bool
    , hidden             :: Bool
    , ignoreCase         :: Bool
    , prompt             :: Maybe String
    , lineCount          :: Maybe Integer
    , height             :: Maybe Integer
    , borderWidth        :: Maybe Integer
    -- color
    , background         :: Maybe String
    , foreground         :: Maybe String
    , selectedBackground :: Maybe String
    , selectedForeground :: Maybe String
    }

dmenuDefaults :: Dmenu
dmenuDefaults =
  Dmenu
    { font = Nothing
    , bottom = False
    , center = False
    , hidden = False
    , ignoreCase = False
    , prompt = Nothing
    , lineCount = Nothing
    , height = Nothing
    , borderWidth = Nothing
    -- color
    , background = Nothing
    , foreground = Nothing
    , selectedBackground = Nothing
    , selectedForeground = Nothing
    }

dmenuDefaults' :: Font -> ColorScheme -> Dmenu
dmenuDefaults' fn cs =
  dmenuDefaults
    { font = Just fn
    -- color
    , background = Just $ base00 cs
    , foreground = Just $ base05 cs
    , selectedBackground = Just $ base08 cs
    , selectedForeground = Just $ base07 cs
    }

constructArgs :: Dmenu -> [String]
constructArgs c =
  concat
    [ ["-b" | bottom c]
    , ["-c" | center c]
    , ["-P" | hidden c]
    , ["-i" | ignoreCase c]
    , concat [["-fn", getXft $ fromJust $ font c] | isJust $ font c]
    , concat [["-p", fromJust $ prompt c] | isJust $ prompt c]
    , concat [["-l", show (fromJust $ lineCount c)] | isJust $ lineCount c]
    , concat [["-h", show (fromJust $ height c)] | isJust $ height c]
    , concat [["-bw", show (fromJust $ borderWidth c)] | isJust $ borderWidth c]
    , concat [["-nb", fromJust $ background c] | isJust $ background c]
    , concat [["-nf", fromJust $ foreground c] | isJust $ foreground c]
    , concat
        [ ["-sb", fromJust $ selectedBackground c]
        | isJust $ selectedBackground c
        ]
    , concat
        [ ["-sf", fromJust $ selectedForeground c]
        | isJust $ selectedForeground c
        ]
    ]

dmenuRun :: MonadIO m => Dmenu -> m ()
dmenuRun c =
  fork $ do
    output <- readProcess "dmenu_path" [] []
    dmenu c (words output) $ \case
      Nothing -> return ()
      Just a -> do
        _ <- spawnCommand a
        return ()

dmenuMap :: MonadIO m => Dmenu -> M.Map String a -> m (Maybe a)
dmenuMap c m =
  dmenu c (M.keys m) $ \case
    Just s -> return (M.lookup s m)
    Nothing -> return Nothing

dmenu :: MonadIO m => Dmenu -> [String] -> (Maybe String -> IO r) -> m r
dmenu c input action =
  liftIO $ do
    out <- runProcessWithInput "dmenu" (constructArgs c) (unlines input)
    if out == "\n"
      then action Nothing
      else action (Just . head . lines $ out)
