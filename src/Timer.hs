module Timer (startTimer, stopTimer, Timer) where

import Control.Concurrent
import Control.Monad.Trans (liftIO, MonadIO)

import Control.Monad.IfElse

data Token = Token
newtype Timer = Timer (MVar Token)

-- |
-- Start a timer.
--
-- Example:
--
-- > t <- startTimer 1000000 $ putStrLn . show
startTimer :: (MonadIO m, Num a) => Int -> (a -> IO ()) -> m Timer
startTimer delay action = liftIO $ do
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
stopTimer :: (MonadIO m) => Timer -> m ()
stopTimer (Timer t) = liftIO $ putMVar t Token

isStopped :: (MonadIO m) => Timer -> m Bool
isStopped (Timer t) = liftIO $ fmap not $ isEmptyMVar t

newTimer :: (MonadIO m) => m Timer
newTimer = liftIO $ fmap Timer $ newEmptyMVar
