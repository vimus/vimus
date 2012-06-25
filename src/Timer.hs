module Timer (
  Timer
, startTimer
, stopTimer
, getCurrentTime
) where

import           Control.Concurrent
import           Control.Monad (when)
import           Data.Time (UTCTime, NominalDiffTime, getCurrentTime,
                            diffUTCTime)

newtype Timer = Timer (MVar Bool)

-- | Start a timer.
--
-- Call given action repeatedly with the passed time since the timer has been
-- started, adjusted by a given offset.  The action is called every second;
-- whenever (passed + offset) is close to (truncate $ passed + offset).
startTimer :: UTCTime -- ^ start time
           -> Double    -- ^ offset in seconds
           -> (Double -> IO ())
           -> IO Timer
startTimer t0 s0 action = do
  m <- newMVar True
  _ <- forkIO $ run t0 (realToFrac s0) (action . realToFrac) m
  return (Timer m)


-- | Run until False is put into the MVar.
run :: UTCTime -> NominalDiffTime -> (NominalDiffTime -> IO ()) -> MVar Bool -> IO ()
run t0 s0 action m = go
  where
    go = do
      t1 <- getCurrentTime
      let s_current = (t1 `diffUTCTime` t0) + s0
          s_next    = s_current + 0.001
      sleep (s_next - s_current)
      isRunning <- takeMVar m
      when (isRunning) $ do
        -- add 0.001 as a tiebreaker to make sure that `truncate` will work as
        -- expected
        action (s_next + 0.001)
        putMVar m isRunning
        go

    -- | Like `threadDelay`, but takes the delay in seconds.
    sleep :: NominalDiffTime -> IO ()
    sleep s = threadDelay $ round (s * 1000000)

-- | Stop timer; block until the timer is stopped.
stopTimer :: Timer -> IO ()
stopTimer (Timer m) = takeMVar m >> putMVar m False
