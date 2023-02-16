module Lib.Effects.Ksql where

import           Data.Kind          (Type)
import           Lib.Effects.Client (Client)

class Ksql (m :: Type -> Type) where
  getObjects :: m [String]

newtype ClientKsqlT (m :: Type -> Type) a = ClientKsqlT (m a)
  deriving newtype ( Functor, Applicative, Monad, Client )

instance (Monad m, Client m) => Ksql (ClientKsqlT m) where
  getObjects :: ClientKsqlT m [String]
  getObjects = pure ["hello"]