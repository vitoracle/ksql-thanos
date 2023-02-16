{-# LANGUAGE UndecidableInstances #-}

module Lib.Effects.Console where
  import Data.Kind ( Type )
  import Control.Monad.Cont (MonadIO, liftIO) 
  
  class Console m where
    out :: Show a => a -> m ()

  newtype ConsoleT (m :: Type -> Type) a = ConsoleT (m a)
    deriving newtype (Functor, Applicative, Monad, MonadIO)

  instance MonadIO m => Console (ConsoleT m) where
    out :: Show a => a -> ConsoleT m ()
    out = ConsoleT . liftIO . print