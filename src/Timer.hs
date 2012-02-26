module Timer (
  Timer
, startTimer
, stopTimer
) where

import           Control.Concurrent
import           Control.Monad.Trans (liftIO, MonadIO)
import           Control.Monad (when)

newtype Timer = Timer (MVar ())

-- | Start a timer.
startTimer :: (MonadIO m, Num a) => Int -> (a -> IO ()) -> m Timer
startTimer delay action = liftIO $ do
  t <- newEmptyMVar
  _ <- forkIO $ go t 1
  return (Timer t)
  where
    -- loop until something is put into the MVar
    go t count = do
      threadDelay delay
      isRunning <- isEmptyMVar t
      when (isRunning) $ do
        action count
        go t (count + 1)

-- | Stop given timer.
stopTimer :: (MonadIO m) => Timer -> m ()
stopTimer (Timer t) = liftIO (putMVar t ())

