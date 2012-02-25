module PlaybackState(onChange, PlaybackState, playState, playStatus, elapsedTime, currentSong) where

import           Prelude hiding (mapM_)
import           Data.Foldable (mapM_)
import           Data.List (find)

import           Control.Monad (when)
import           Control.Monad.State (StateT, liftIO, evalStateT, gets, modify)

import qualified Network.MPD as MPD hiding (withMPD)
import qualified Network.MPD.Commands.Extensions as MPDE
import           Network.MPD (MPD(), Seconds, Song, Id)

import           Timer (Timer)
import qualified Timer

import           Type ()

data PlaybackState = PlaybackState {
    playState    :: MPD.State
  , playStatus   :: MPD.Status
  , elapsedTime_ :: (Double, Seconds)
  , currentSong  :: Maybe MPD.Song
} deriving Show

data State = State {
  playlist :: [Song]
, timer    :: Maybe Timer
}

elapsedTime :: PlaybackState -> (Seconds, Seconds)
elapsedTime s = case elapsedTime_ s of (c, t) -> (round c, t)

-- | Execute action on each change of the playback state.
onChange :: ([Song] -> IO ()) -> (PlaybackState -> IO ()) -> MPD ()
onChange plChanged action = do
  MPDE.getPlaylist >>= liftIO . plChanged
  pl <- MPDE.getPlaylist
  liftIO (plChanged pl)
  evalStateT go (State pl Nothing)
  where

    go = do
      gets playlist >>= queryState action

      r <- MPD.idle [MPD.PlayerS, MPD.OptionsS, MPD.PlaylistS]

      when (MPD.PlaylistS `elem` r) $ do
        pl <- MPDE.getPlaylist
        liftIO (plChanged pl)
        modify (\st -> st {playlist = pl})
      go

-- | Find first song with given id.
findSongWithId :: Id -> [Song] -> Maybe Song
findSongWithId songId = find ((== Just songId) . MPD.sgId)

-- |
-- Query the playback state and call given action.  If a song is currently
-- being played, start a timer, that repeatedly updates the elapsed time and
-- calls given action.  The timer, if any, is returned and the caller is
-- responsible for stopping it.
queryState :: (PlaybackState -> IO ()) -> [Song] -> StateT State MPD ()
queryState action pl = do

  -- stop old timer
  gets timer >>= mapM_ Timer.stop
  modify (\st -> st {timer = Nothing})

  -- query state
  status <- MPD.status
  let song = maybe (const Nothing) findSongWithId (MPD.stSongID status) pl

  -- put state into var
  let s = PlaybackState (MPD.stState status) status (MPD.stTime status) song
  liftIO $ action s

  -- start timer, if playing
  when (playState s == MPD.Playing) $ do
    t <- Timer.start 1000000 $ \count -> do
      action (updateElapsedTime s count)
    modify (\st -> st {timer = Just t})

-- |
-- Increase elapsed time of given playback state by given seconds.
updateElapsedTime :: PlaybackState -> Double -> PlaybackState
updateElapsedTime s seconds = s {elapsedTime_ = (timeElapsed + seconds, timeTotal)}
  where
    (timeElapsed, timeTotal) = elapsedTime_ s
