module PlaybackState (
  onChange
, elapsedTime
) where

import           Prelude hiding (mapM_)
import           Data.Foldable (mapM_)

import           Control.Monad (when, forever)
import           Control.Applicative
import           Control.Monad.State.Strict (liftIO, evalStateT, gets, modify)

import           Data.Default

import           Data.Maybe (fromMaybe)

import qualified Network.MPD as MPD hiding (withMPD)
import           Network.MPD (MPD(), Seconds, Song)
import qualified Network.MPD.Applicative as MPDA

import           Timer

import           Instances ()

data State = State {
  timer       :: Maybe Timer
, currentSong :: Maybe Song
}

instance Default State where
  def = State def def

elapsedTime :: MPD.Status -> (Seconds, Seconds)
elapsedTime s = case MPD.stTime s of Just (c, t) -> (truncate c, truncate t); _ -> (0, 0)

-- | Execute given actions on changes.
onChange :: IO () -> (Maybe Song -> IO ()) -> (Maybe Song -> MPD.Status -> IO ()) -> MPD ()
onChange plChanged songChanged statusChanged =
  evalStateT (updatePlaylist >> updateTimerAndCurrentSong >> idle) def
  where

    -- Wait for changes.
    idle = forever $ do
      r <- MPD.idle [MPD.PlaylistS, MPD.PlayerS, MPD.OptionsS, MPD.UpdateS, MPD.MixerS]
      if (MPD.PlaylistS `elem` r)
        then do
          -- If the playlist changed, we have to run both, `updatePlaylist` and
          -- `updateTimerAndCurrentSong`.
          --
          -- `updateTimerAndCurrentSong` is necessary to emit
          -- EvCurrentSongChanged.  This is necessary even if the song did not
          -- really change, because it's position within the playlist may have
          -- changed; and we only use a position to mark the current element of
          -- a ListWidget (say the currently played song here).
          updatePlaylist
          updateTimerAndCurrentSong
        else do
          -- MPD.PlayerS | MPD.OptionsS | MPD.UpdateS | MPD.MixerS
          updateTimerAndCurrentSong

    -- Call `plChanged` action.
    updatePlaylist = liftIO plChanged

    -- Query MPD's status and call `statusChanged`, and if current song changed
    -- `songChanged`.  If a song is currently being played, start a timer, that
    -- repeatedly updates the elapsed time and calls `statusChanged`.
    updateTimerAndCurrentSong = do

      -- stop old timer (if any)
      gets timer >>= mapM_ (liftIO . stopTimer)
      modify (\st -> st {timer = Nothing})

      -- query status and call `statusChanged`
      (song, status) <- MPDA.runCommand $ (,) <$> MPDA.currentSong <*> MPDA.status
      liftIO (statusChanged song status)

      -- call `songChanged` if necessary
      oldSong <- gets currentSong
      when (oldSong /= song) $ do
        modify (\st -> st {currentSong = song})
        liftIO (songChanged song)

      -- start timer, if playing
      when (MPD.stState status == MPD.Playing) $ do
        t0 <- liftIO getPOSIXTime
        let (s0, _) = fromMaybe (0, 0) (MPD.stTime status)
        t <- liftIO . startTimer t0 s0 $ \elapsed -> do
          statusChanged song (updateElapsedTime status elapsed)
        modify (\st -> st {timer = Just t})

-- | Set elapsed time of given status to given seconds.
updateElapsedTime :: MPD.Status -> Double -> MPD.Status
updateElapsedTime st seconds = st {MPD.stTime = Just (seconds, timeTotal)}
  where
    (_, timeTotal) = fromMaybe (0, 0) (MPD.stTime st)
