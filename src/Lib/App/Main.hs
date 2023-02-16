module Lib.App.Main (start) where
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..))
import           Lib.Effects.Console      (Console, ConsoleT (..), out)
import           Lib.App.Config       (Config)
import           Lib.Effects.Client   (Client, ClientT (..))
import           Lib.Effects.Ksql     (Ksql (getObjects), ClientKsqlT (..))
import Lib.Core (app)
import Data.Has

newtype AppM a = AppM { runApp :: ReaderT Config IO a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadReader Config)
    via ( ReaderT Config IO )
  deriving Console
    via ConsoleT AppM
  deriving Client
    via ClientT AppM
  deriving Ksql
    via ClientKsqlT AppM

start :: Config -> IO ()
start = runReaderT $ runApp app
