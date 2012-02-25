module PlaybackState(onChange, PlaybackState, playState, playStatus, elapsedTime, currentSong) where

import           Prelude hiding (mapM_)
import           Data.Foldable (mapM_)
import           Data.List (find)

import           Control.Monad (when, forever)
import           Control.Monad.State (liftIO, evalStateT, gets, modify)

import           Data.Default

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

instance Default State where
  def = State def def

elapsedTime :: PlaybackState -> (Seconds, Seconds)
elapsedTime s = case elapsedTime_ s of (c, t) -> (round c, t)

-- | Execute given actions on changes.
onChange :: ([Song] -> IO ()) -> (PlaybackState -> IO ()) -> MPD ()
onChange plChanged action =
  evalStateT (updatePlaylist >> updateTimer >> idle) def
  where

    -- Wait for changes.
    idle = forever $ do
      r <- MPD.idle [MPD.PlayerS, MPD.OptionsS, MPD.PlaylistS]
      when (MPD.PlaylistS `elem` r)
        updatePlaylist
      when (MPD.PlayerS `elem` r || MPD.OptionsS `elem` r)
        updateTimer

    -- Fetch current playlist, update state, and call `plChanged` action.
    updatePlaylist = do
      pl <- MPDE.getPlaylist
      modify (\st -> st {playlist = pl})
      liftIO $ plChanged pl

    -- Query the playback state and call `action`.  If a song is currently
    -- being played, start a timer, that repeatedly updates the elapsed time
    -- and calls `action`.
    updateTimer = do

      -- stop old timer (if any)
      gets timer >>= mapM_ Timer.stop
      modify (\st -> st {timer = Nothing})

      -- query state and call action
      status <- MPD.status
      pl <- gets playlist
      let song = maybe (const Nothing) findSongWithId (MPD.stSongID status) pl
      let s = PlaybackState (MPD.stState status) status (MPD.stTime status) song
      liftIO $ action s

      -- start timer, if playing
      when (playState s == MPD.Playing) $ do
        t <- Timer.start 1000000 $ \count -> do
          action (updateElapsedTime s count)
        modify (\st -> st {timer = Just t})

-- | Find first song with given id.
findSongWithId :: Id -> [Song] -> Maybe Song
findSongWithId songId = find ((== Just songId) . MPD.sgId)

-- | Increase elapsed time of given playback state by given seconds.
updateElapsedTime :: PlaybackState -> Double -> PlaybackState
updateElapsedTime s seconds = s {elapsedTime_ = (timeElapsed + seconds, timeTotal)}
  where
    (timeElapsed, timeTotal) = elapsedTime_ s
