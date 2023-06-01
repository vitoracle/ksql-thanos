{-# LANGUAGE UndecidableInstances #-}

module Lib.App.Main (start) where

import Control.Monad.Identity (IdentityT (runIdentityT))
import Control.Monad.Reader
  ( MonadIO,
    MonadReader (ask),
    MonadTrans,
    ReaderT (..),
    lift,
  )
import Control.Monad.Trans.Compose (ComposeT (ComposeT))
import Control.Monad.Trans.Compose.Infix
import Control.Monad.Trans.Compose.Transparent
  ( TransparentT,
    runTransparentT,
  )
import Control.Monad.Trans.Control (MonadTransControl (StT))
import Control.Monad.Trans.Elevator (Elevator (Ascend))
import Data.Has
import Data.Kind
import Lib.App.Config (Config)
import Lib.Effects.Client (Client, ClientT (..))

newtype AppT m a = AppT {unAppT :: (TransparentT .|> ReaderT String) m a}
  deriving newtype (Functor, Monad, Applicative, MonadIO)
  deriving newtype (MonadTrans, MonadTransControl)
  deriving newtype (MonadReader String)

type CliStackT = TransparentT .|> ReaderT Bool

newtype CliM a = CliM {unCli :: AppT ((TransparentT .|> ReaderT Bool) IO) a}
  deriving newtype (Functor, Monad, Applicative, MonadIO, MonadReader String)

start :: IO ()
start = do
  -- let another = runTransparentT ./> runReader ./> runConsoleT $ unAppT program
  print "done"
  where
    runReader :: ReaderT String m a -> m a
    runReader c = runReaderT c "a"
