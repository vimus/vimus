module Timer (start, stop, Timer) where

import Control.Concurrent
import Control.Monad.Trans (liftIO, MonadIO)

import Control.Monad.IfElse (unlessM)

data Token = Token
newtype Timer = Timer (MVar Token)

-- |
-- Start a timer.
--
-- Example:
--
-- > t <- Timer.start 1000000 $ putStrLn . show
start :: (MonadIO m, Num a) => Int -> (a -> IO ()) -> m Timer
start delay action = liftIO $ do
  timer <- newTimer
  _ <- forkIO $ loop timer 1
  return timer
  where
    loop timer count = do
      threadDelay delay
      unlessM (isStopped timer) $ do
        action count
        loop timer $ count + 1

-- |
-- Stop given timer.
stop :: (MonadIO m) => Timer -> m ()
stop (Timer t) = liftIO $ putMVar t Token

isStopped :: (MonadIO m) => Timer -> m Bool
isStopped (Timer t) = liftIO $ fmap not $ isEmptyMVar t

newTimer :: (MonadIO m) => m Timer
newTimer = liftIO $ fmap Timer newEmptyMVar
