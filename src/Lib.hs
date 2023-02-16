module Lib (runApp) where

import Lib.App.Main (start)
import Lib.App.Config (Config (..))

runApp :: IO ()
runApp = start (Config "abc")
