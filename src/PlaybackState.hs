module PlaybackState(onChange, PlaybackState, playState, playStatus, elapsedTime, currentSong) where

import           Control.Monad.Trans (liftIO, MonadIO)
import           Data.Foldable (for_)
import           Data.List (find)

import qualified Network.MPD as MPD hiding (withMPD)
import qualified Network.MPD.Commands.Extensions as MPDE
import           Network.MPD (MPD(), Seconds, Song, Id)

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
onChange :: ([Song] -> IO ()) -> (PlaybackState -> IO ()) -> MPD ()
onChange plChanged action = do
  MPDE.getPlaylist >>= liftIO . plChanged
  pl <- MPDE.getPlaylist
  liftIO (plChanged pl)
  go pl
  where
    go pl = do
      timer <- queryState pl action
      r <- MPD.idle [MPD.PlayerS, MPD.OptionsS, MPD.PlaylistS]

      pl_ <- case MPD.PlaylistS `elem` r of
        True -> do
          x <- MPDE.getPlaylist
          liftIO (plChanged x)
          return x
        False ->
          return pl

      for_ timer Timer.stop
      go pl_

-- | Find first song with given id.
findSongWithId :: Id -> [Song] -> Maybe Song
findSongWithId songId = find ((== Just songId) . MPD.sgId)

-- |
-- Query the playback state and call given action.  If a song is currently
-- being played, start a timer, that repeatedly updates the elapsed time and
-- calls given action.  The timer, if any, is returned and the caller is
-- responsible for stopping it.
queryState :: [Song] -> (PlaybackState -> IO ()) -> MPD (Maybe Timer)
queryState pl action = do

  -- query state
  status <- MPD.status
  let song = maybe (const Nothing) findSongWithId (MPD.stSongID status) pl

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
