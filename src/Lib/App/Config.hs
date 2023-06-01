{-# LANGUAGE DeriveAnyClass #-}

module Lib.App.Config where

import Data.Generics.Product.Typed (HasType)
import GHC.Generics
import Network.HTTP.Client (Manager)

data Config = Config {a :: String}
