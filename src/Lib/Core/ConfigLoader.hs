{-# LANGUAGE UndecidableInstances #-}

module Lib.Core.ConfigLoader where

import Control.Monad.Cont
import Control.Monad.Trans.Compose (ComposeT (ComposeT))
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Data.Kind
import Lib.Core.Config

class Monad m => ConfigLoader (m :: Type -> Type) where
  loadConfig :: m Config

instance
  ( Monad (t m),
    MonadTrans t,
    ConfigLoader m
  ) =>
  ConfigLoader (Elevator t m)
  where
  loadConfig = lift loadConfig

deriving via
  Elevator t1 (t2 (m :: Type -> Type))
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m)),
      MonadTransControl t1,
      ConfigLoader (t2 m)
    ) =>
    ConfigLoader (ComposeT t1 t2 m)