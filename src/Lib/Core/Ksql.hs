{-# LANGUAGE UndecidableInstances #-}

module Lib.Core.Ksql where

import Control.Monad.Cont
import Control.Monad.Identity (IdentityT)
import Control.Monad.Trans.Compose (ComposeT (ComposeT))
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator (Elevator (Ascend))
import Data.Kind (Type)

class (Monad m) => Ksql (m :: Type -> Type) where
  listQueries :: m [String]

deriving via
  KsqlT (t2 (m :: Type -> Type))
  instance
    (Monad (t2 m)) => Ksql (ComposeT KsqlT t2 m)

instance
  ( Monad (t m),
    MonadTrans t,
    Ksql m
  ) =>
  Ksql (Elevator t m)
  where
  getObjects = lift getObjects

deriving via
  Elevator t1 (t2 (m :: Type -> Type))
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m)),
      MonadTransControl t1,
      Ksql (t2 m)
    ) =>
    Ksql (ComposeT t1 t2 m)

newtype KsqlT (m :: Type -> Type) a = KsqlT {unKsqlT :: IdentityT m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadTrans,
      MonadTransControl
    )

instance (Monad m) => Ksql (KsqlT m) where
  getObjects :: KsqlT m [String]
  getObjects = pure ["hello"]
