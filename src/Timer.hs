module Timer (
  Timer
, startTimer
, stopTimer
) where

import           Control.Concurrent
import           Control.Monad.Trans (liftIO, MonadIO)
import           Control.Monad (when)

newtype Timer = Timer (MVar Bool)

-- | Start a timer.
startTimer :: (MonadIO m, Num a) => Int -> (a -> IO ()) -> m Timer
startTimer delay action = liftIO $ do
  m <- newMVar True
  _ <- forkIO $ go m 1
  return (Timer m)
  where
    -- run until False is put into the MVar
    go m count = do
      threadDelay delay
      isRunning <- takeMVar m
      when (isRunning) $ do
        action count
        putMVar m isRunning
        go m (count + 1)

-- | Stop timer; block until the timer is stopped.
stopTimer :: (MonadIO m) => Timer -> m ()
stopTimer (Timer m) = liftIO (takeMVar m >> putMVar m False)
