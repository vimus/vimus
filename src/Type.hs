{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Common types an instances.
module Type where

import           Data.Default
import           Control.Monad.State.Strict
import           Network.MPD.Core

instance MonadMPD (StateT a MPD) where
  getVersion  = lift   getVersion
  open        = lift   open
  close       = lift   close
  send        = lift . send
  getHandle   = lift   getHandle
  setPassword = lift . setPassword
  getPassword = lift   getPassword

data WindowSize = WindowSize {
  windowSizeY :: Int
, windowSizeX :: Int
} deriving (Eq, Show)

instance Default WindowSize where
  def = WindowSize 25 80

class Renderable a where
  renderItem :: a -> String

instance Renderable String where
  renderItem = id
