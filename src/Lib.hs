module Lib (runApp) where

import Lib.App.Main (start)

runApp :: IO ()
runApp = print "hey"
