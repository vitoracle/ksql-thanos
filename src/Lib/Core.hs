module Lib.Core (app) where

import Lib.Effects.Console
import Lib.Effects.Ksql
import Control.Monad.RWS (MonadReader)

newtype Config = Config String

app :: (Ksql m, Console m, Monad m) => m ()
app = do
  objs <- getObjects
  out objs