module Lib.Core.Config where

newtype Config = Config String
  deriving newtype (Show)