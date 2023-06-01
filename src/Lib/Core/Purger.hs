module Lib.Core.Purger where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text
import Lib.Core.Config
import Lib.Core.ConfigLoader
import Lib.Core.Ksql

newtype PurgerT m a = PurgerT {unPurgerT :: ReaderT Config m a}

start :: (MonadLogger m, Ksql m, ConfigLoader m) => m ()
start =
  do
    cfg <- loadConfig
    $logInfo $ "Started purger with " <> pack (show cfg)
  where
    purge :: (MonadLogger m, MonadReader Config m, Ksql m) => m ()
    purge = do
      cfg <- ask
      pure ()