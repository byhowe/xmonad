module Config.Terminal
  ( Terminal(..)
  , alacritty
  , spawnTerminal
  , shellCommand
  , printToTerminal
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust, isJust)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.Process (spawnProcess)
import Text.Printf (printf)

data Terminal =
  Terminal
    { command     :: String
    , classFlag   :: String
    , commandFlag :: String
    , cls         :: Maybe String
    , cmd         :: Maybe [String]
    }

alacritty :: Terminal
alacritty =
  Terminal
    { command = "alacritty"
    , classFlag = "--class"
    , commandFlag = "e"
    , cls = Nothing
    , cmd = Nothing
    }

constructArgs :: Terminal -> [String]
constructArgs t =
  concat
    [ concat [[classFlag t, fromJust $ cls t] | isJust $ cls t]
    , [commandFlag t | isJust $ cmd t]
    , concat [fromJust $ cmd t | isJust $ cmd t]
    ]

spawnTerminal :: MonadIO m => Terminal -> m ()
spawnTerminal t = do
  let flags = constructArgs t
  _ <- liftIO . spawnProcess (command t) $ flags
  return ()

shellCommand :: Terminal -> String
shellCommand = concat . constructArgs

printToTerminal :: MonadIO m => Terminal -> String -> m ()
printToTerminal t s =
  liftIO $ do
    temp <- getTemporaryDirectory
    let file = temp </> "xmonad-terminal"
    writeFile file s
    spawnTerminal $
      t {cmd = Just ["sh", "-c", printf "cat %s && read _ && rm %s" file file]}
    return ()
