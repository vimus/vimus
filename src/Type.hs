{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Common types an instances.
module Type where

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
