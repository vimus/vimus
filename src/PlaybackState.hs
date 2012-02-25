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
  playlist :: [Song]
, timer    :: Maybe Timer
}

instance Default State where
  def = State def def

elapsedTime :: MPD.Status -> (Seconds, Seconds)
elapsedTime s = case MPD.stTime s of (c, t) -> (round c, t)

-- | Execute given actions on changes.
onChange :: ([Song] -> IO ()) -> (Maybe Song -> MPD.Status -> IO ()) -> MPD ()
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
      liftIO $ action song status

      -- start timer, if playing
      when (MPD.stState status == MPD.Playing) $ do
        t <- Timer.start 1000000 $ \count -> do
          action song (updateElapsedTime status count)
        modify (\st -> st {timer = Just t})

-- | Find first song with given id.
findSongWithId :: Id -> [Song] -> Maybe Song
findSongWithId songId = find ((== Just songId) . MPD.sgId)

-- | Increase elapsed time of given status by given seconds.
updateElapsedTime :: MPD.Status -> Double -> MPD.Status
updateElapsedTime s seconds = s {MPD.stTime = (timeElapsed + seconds, timeTotal)}
  where
    (timeElapsed, timeTotal) = MPD.stTime s
