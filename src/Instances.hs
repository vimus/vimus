{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | orphan instances
module Instances where

import           Control.Monad.State.Strict
import           Network.MPD.Core

instance MonadMPD (StateT a MPD) where
  getVersion  = lift   getVersion
  open        = lift   open
  close       = lift   close
  send        = lift . send
  setPassword = lift . setPassword
  getPassword = lift   getPassword
