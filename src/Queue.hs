-- | A simple unbounded queue.
--
-- All operations are non-blocking.
module Queue (
  Queue
, newQueue
, putQueue
, takeAllQueue
) where

import           Control.Exception (mask_)
import           Control.Concurrent.MVar
import           Control.Applicative

newtype Queue a = Queue (MVar [a])

-- | Create an empty queue.
newQueue :: IO (Queue a)
newQueue = Queue <$> newMVar []

-- | Put an element into the queue.
putQueue :: Queue a -> a -> IO ()
putQueue (Queue m) x = mask_ $ takeMVar m >>= putMVar m . (x:)

-- | Take all elements from the queue.
takeAllQueue :: Queue a -> IO [a]
takeAllQueue (Queue m) = reverse <$> swapMVar m []
