module PlaybackState(onChange, elapsedTime) where

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

data State = State {
  playlist    :: [Song]
, timer       :: Maybe Timer
, currentSong :: Maybe Song
}

instance Default State where
  def = State def def def

elapsedTime :: MPD.Status -> (Seconds, Seconds)
elapsedTime s = case MPD.stTime s of (c, t) -> (round c, t)

-- | Execute given actions on changes.
onChange :: ([Song] -> IO ()) -> (Maybe Song -> IO ()) -> (Maybe Song -> MPD.Status -> IO ()) -> MPD ()
onChange plChanged songChanged statusChanged =
  evalStateT (updatePlaylist >> updateTimerAndCurrentSong >> idle) def
  where

    -- Wait for changes.
    idle = forever $ do
      r <- MPD.idle [MPD.PlayerS, MPD.OptionsS, MPD.PlaylistS]
      when (MPD.PlaylistS `elem` r)
        updatePlaylist
      when (MPD.PlayerS `elem` r || MPD.OptionsS `elem` r)
        updateTimerAndCurrentSong

    -- Fetch current playlist, update state, and call `plChanged` action.
    updatePlaylist = do
      pl <- MPDE.getPlaylist
      modify (\st -> st {playlist = pl})
      liftIO $ plChanged pl

    -- Query MPD's status and call `statusChanged`, and if current song changed
    -- `songChanged`.  If a song is currently being played, start a timer, that
    -- repeatedly updates the elapsed time and calls `statusChanged`.
    updateTimerAndCurrentSong = do

      -- stop old timer (if any)
      gets timer >>= mapM_ Timer.stop
      modify (\st -> st {timer = Nothing})

      -- query status and call actions
      status <- MPD.status
      pl <- gets playlist
      let song = maybe def findSongWithId (MPD.stSongID status) pl
      liftIO (statusChanged song status)

      oldSong <- gets currentSong
      when (oldSong /= song) $ do
        modify (\st -> st {currentSong = song})
        liftIO (songChanged song)

      -- start timer, if playing
      when (MPD.stState status == MPD.Playing) $ do
        t <- Timer.start 1000000 $ \count -> do
          statusChanged song (updateElapsedTime status count)
        modify (\st -> st {timer = Just t})

-- | Find first song with given id.
findSongWithId :: Id -> [Song] -> Maybe Song
findSongWithId songId = find ((== Just songId) . MPD.sgId)

-- | Increase elapsed time of given status by given seconds.
updateElapsedTime :: MPD.Status -> Double -> MPD.Status
updateElapsedTime s seconds = s {MPD.stTime = (timeElapsed + seconds, timeTotal)}
  where
    (timeElapsed, timeTotal) = MPD.stTime s
