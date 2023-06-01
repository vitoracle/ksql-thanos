{-# LANGUAGE UndecidableInstances #-}

module Lib.Effects.Client where

import Control.Exception (throwIO)
import Control.Lens
import Control.Monad.Cont (MonadIO, liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..))
import Data.Has
import Data.Kind (Type)
import Network.HTTP.Client (Manager)
import Servant.Client (ClientEnv, ClientM, mkClientEnv, runClientM)

class Client (m :: Type -> Type) where
  runClientM' :: ClientEnv -> ClientM a -> m a

newtype ClientT (m :: Type -> Type) a = ClientT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m, MonadReader e m, Has Manager e) => Client (ClientT m) where
  runClientM' :: ClientEnv -> ClientM a -> ClientT m a
  runClientM' = (.) (liftIO . (=<<) (either throwIO pure)) . flip runClientM
