{-# LANGUAGE TypeSynonymInstances #-}
module Widget where

import Control.Monad.Trans (MonadIO)
import Text.Printf (printf)

import qualified Network.MPD as MPD hiding (withMPD)

import UI.Curses (Window)

import ListWidget (ListWidget)
import qualified ListWidget

class Widget a where
  render :: (MonadIO m) => Window -> a -> m ()

type SongListWidget = ListWidget MPD.Song

instance Widget SongListWidget where
  render window l = do
    ListWidget.render l renderOne window
    where
      renderOne :: MPD.Song -> String
      renderOne song = printf "%s - %s - (%2d,%2d) - %s" (MPD.sgArtist song) (MPD.sgAlbum song) currentTrack totalTracks (MPD.sgTitle song)
        where (currentTrack, totalTracks) = MPD.sgTrack song
