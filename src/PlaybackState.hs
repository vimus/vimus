module PlaybackState(onChange, PlaybackState(..)) where

import Data.Foldable (for_)

import Control.Monad (forever)
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Network.MPD as MPD
import Network.MPD (MPD(), Seconds)

import Timer

data PlaybackState = PlaybackState {
    playState   :: MPD.State
  , elapsedTime :: (Seconds, Seconds)
  , currentSong :: Maybe MPD.Song
} deriving Show

-- |
-- Execute action on each change of the playback status.
onChange :: (PlaybackState -> IO ()) -> MPD ()
onChange action = do
  var <- liftIO newEmptyMVar

  -- execute action on each change of var (in a separate thread)
  _ <- liftIO . forkIO $ forever $ do
    state <- takeMVar var
    action state

  -- wait for changes and put them into var
  forever $ do
    timer <- queryState var
    doUntil (MPD.Player `elem`) MPD.idle
    for_ timer stopTimer

-- |
-- Execute given action repeatedly until its result satisfies given predicate.
doUntil :: Monad m => (a -> Bool) -> m a -> m ()
doUntil predicate action = do
  r <- action
  if predicate r
    then return ()
    else doUntil predicate action

-- |
-- Query the playback state and put the result into given MVar.  If a song is
-- currently being played, start a timer, that repeatedly updates the elapsed
-- time and puts the result into given MVar.  The timer, if any, is returned
-- and the caller is responsible for stopping it.
queryState :: MVar PlaybackState -> MPD (Maybe Timer)
queryState var = do

  -- query state
  status <- MPD.status
  song   <- MPD.currentSong

  -- put state into var
  let state = PlaybackState (MPD.stState status) (MPD.stTime status) song
  liftIO $ putMVar var state

  -- start timer, if playing
  if playState state == MPD.Playing
    then do
      timer <- startTimer 1000000 $ \count -> do
        putMVar var $ updateElapsedTime state count
      return $ Just timer
    else
      return Nothing

-- |
-- Increase elapsed time of given playback state by given seconds.
updateElapsedTime :: PlaybackState -> Seconds -> PlaybackState
updateElapsedTime state seconds = state {elapsedTime = (timeElapsed + seconds, timeTotal)}
  where
    (timeElapsed, timeTotal) = elapsedTime state
