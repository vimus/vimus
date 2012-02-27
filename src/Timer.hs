module Timer (
  Timer
, startTimer
, stopTimer
, getClockTime
) where

import           Control.Concurrent
import           Control.Monad (when)
import           System.Time (ClockTime(..), getClockTime)

newtype Timer = Timer (MVar Bool)

-- | Start a timer.
--
-- Call given action repeatedly with the passed time since the timer has been
-- started, adjusted by a given offset.  The action is called every second;
-- whenever (passed + offset) is close to (truncate $ passed + offset).
startTimer :: ClockTime -- ^ start time
           -> Double    -- ^ offset in seconds
           -> (Double -> IO ())
           -> IO Timer
startTimer t0 s0 action = do
  m <- newMVar True
  _ <- forkIO $ run t0 s0 action m
  return (Timer m)


-- | Run until False is put into the MVar.
run :: ClockTime -> Double -> (Double -> IO ()) -> MVar Bool -> IO ()
run t0 s0 action m = go
  where
    go = do
      t1 <- getClockTime
      let s_current = s0 + (t1 `minus` t0)
          s_next = fromIntegral (ceiling (s_current + 0.001) :: Int)
      sleep (s_next - s_current)
      isRunning <- takeMVar m
      when (isRunning) $ do
        -- add 0.001 as a tiebreaker to make sure that `truncate` will work as
        -- expected
        action (s_next + 0.001)
        putMVar m isRunning
        go

    -- | Like `threadDelay`, but takes the delay in seconds.
    sleep :: Double -> IO ()
    sleep s = threadDelay $ round (s * 1000000)

    -- | Time difference in seconds.
    minus :: ClockTime -> ClockTime -> Double
    minus (TOD s1 p1) (TOD s2 p2) = s + (p * 1.0e-12)
      where
        s = fromIntegral (s1 - s2)
        p = fromIntegral (p1 - p2)

-- | Stop timer; block until the timer is stopped.
stopTimer :: Timer -> IO ()
stopTimer (Timer m) = takeMVar m >> putMVar m False
