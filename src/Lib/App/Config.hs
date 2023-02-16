{-# LANGUAGE DeriveAnyClass #-}

module Lib.App.Config where
import Network.HTTP.Client (Manager)
import GHC.Generics
import Data.Generics.Product.Typed ( HasType )

data Config 
  = Config 
    { manager :: Manager
    }
  deriving (Generic, HasType Manager)