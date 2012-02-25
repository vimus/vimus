module PlaybackState(onChange, PlaybackState, playState, playStatus, elapsedTime, currentSong) where

import Data.Foldable (for_)

import Control.Monad
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Network.MPD as MPD hiding (withMPD)
import Network.MPD (MPD(), Seconds)

import           Timer (Timer)
import qualified Timer

data PlaybackState = PlaybackState {
    playState    :: MPD.State
  , playStatus   :: MPD.Status
  , elapsedTime_ :: (Double, Seconds)
  , currentSong  :: Maybe MPD.Song
} deriving Show

elapsedTime :: PlaybackState -> (Seconds, Seconds)
elapsedTime s = case elapsedTime_ s of (c, t) -> (round c, t)

-- | Execute action on each change of the playback state.
onChange :: (PlaybackState -> IO ()) -> MPD ()
onChange action = forever $ do
  timer <- queryState action
  _ <- MPD.idle [MPD.PlayerS, MPD.OptionsS]
  for_ timer Timer.stop

-- |
-- Query the playback state and call given action.  If a song is currently
-- being played, start a timer, that repeatedly updates the elapsed time and
-- calls given action.  The timer, if any, is returned and the caller is
-- responsible for stopping it.
queryState :: (PlaybackState -> IO ()) -> MPD (Maybe Timer)
queryState action = do

  -- query state
  status <- MPD.status
  song   <- MPD.currentSong

  -- put state into var
  let state = PlaybackState (MPD.stState status) status (MPD.stTime status) song
  liftIO $ action state

  -- start timer, if playing
  if playState state == MPD.Playing
    then do
      timer <- Timer.start 1000000 $ \count -> do
        action (updateElapsedTime state count)
      return $ Just timer
    else
      return Nothing

-- |
-- Increase elapsed time of given playback state by given seconds.
updateElapsedTime :: PlaybackState -> Double -> PlaybackState
updateElapsedTime state seconds = state {elapsedTime_ = (timeElapsed + seconds, timeTotal)}
  where
    (timeElapsed, timeTotal) = elapsedTime_ state
