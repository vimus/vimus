{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Widget where

import Control.Monad.Trans (MonadIO)
import Text.Printf (printf)

import qualified Network.MPD as MPD hiding (withMPD)

import UI.Curses (Window)

import ListWidget (ListWidget)
import qualified ListWidget

import qualified Song

import System.FilePath.Posix (takeFileName)

class Widget a where
  render :: (MonadIO m) => Window -> a -> m ()
  title  :: a -> String

type ContentListWidget = ListWidget MPD.LsResult

instance Widget ContentListWidget where
  render window l = do
    ListWidget.render l renderOne window
    where
      renderOne :: MPD.LsResult -> String
      renderOne (MPD.LsDirectory path) = "[" ++ takeFileName path ++ "]"
      renderOne (MPD.LsFile song)      = printf "%s - %s - %s - %s" (Song.artist song) (Song.album song) (Song.track song) (Song.title song)
      renderOne (MPD.LsPlaylist list)  = "(" ++ takeFileName list ++ ")"

  title = ListWidget.breadcrumbs
