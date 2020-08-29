{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Config.Fullscreen
  ( fullscreen
  ) where

import Control.Monad (when)
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen)
import XMonad.Layout.Fullscreen
    ( FullscreenFull
    , FullscreenMessage (..)
    , fullscreenEventHook
    , fullscreenFull
    , fullscreenManageHook
    )
import XMonad.Layout.LayoutModifier (LayoutModifier (..), ModifiedLayout (..))

data Fullscreen w =
  Fullscreen
  deriving (Read, Show)

instance LayoutModifier Fullscreen Window where
  handleMess _ m
    | Just (AddFullscreen w) <- fromMessage m = do
      setBorder False w
      return . Just $ Fullscreen
    | Just (RemoveFullscreen w) <- fromMessage m = do
      setBorder True w
      return . Just $ Fullscreen
    | otherwise = return Nothing

setBorder :: Bool -> Window -> X ()
setBorder set w = do
  bw <- asks (borderWidth . config)
  withDisplay $ \d ->
    liftIO $ do
      cw <- wa_border_width `fmap` getWindowAttributes d w
      if set
        then when (cw == 0) $ setWindowBorderWidth d w bw
        else when (cw /= 0) $ setWindowBorderWidth d w 0

fullscreen ::
     LayoutClass l Window
  => XConfig l
  -> XConfig (ModifiedLayout Fullscreen (ModifiedLayout FullscreenFull l))
fullscreen c =
  ewmhFullscreen $
  c
    { XMonad.layoutHook =
        ModifiedLayout Fullscreen $ fullscreenFull $ XMonad.layoutHook c
    , XMonad.handleEventHook = XMonad.handleEventHook c <+> fullscreenEventHook
    , XMonad.manageHook = XMonad.manageHook c <+> fullscreenManageHook
    }
