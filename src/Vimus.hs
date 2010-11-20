{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Vimus
( Vimus
, ProgramState (..)
, CurrentView (..)
, SongListWidget
, setCurrentView
, modifyCurrentSongList
, withCurrentSongList
, withCurrentSong
)
where

import Control.Monad.State

import Network.MPD.Core
import qualified Network.MPD as MPD hiding (withMPD)

import UI.Curses (Window)

import ListWidget (ListWidget)
import qualified ListWidget

data CurrentView = Playlist | Library

type SongListWidget = ListWidget MPD.Song

data ProgramState = ProgramState {
  currentView       :: CurrentView
, playlistWidget    :: SongListWidget
, libraryWidget     :: SongListWidget
, mainWindow        :: Window
, statusLine        :: Window
, getLastSearchTerm :: String
}


instance MonadMPD (StateT ProgramState MPD) where
  open        = lift open
  close       = lift close
  send        = lift . send
  getPassword = lift getPassword

type Vimus a = StateT ProgramState MPD a

{-
newtype Vimus a = Vimus {
  runVimus :: StateT ProgramState MPD a
} deriving (Monad, Functor, MonadIO, MonadState ProgramState, MonadError MPDError, MonadMPD)
-}


setCurrentView :: CurrentView -> Vimus ()
setCurrentView v = modify (\state -> state { currentView = v })


-- | Modify currently selected song list by applying given function.
modifyCurrentSongList :: (MonadState ProgramState m) => (SongListWidget -> SongListWidget) -> m ()
modifyCurrentSongList f = do
  state <- get
  case currentView state of
    Playlist -> put state { playlistWidget = f $ playlistWidget state }
    Library  -> put state { libraryWidget  = f $ libraryWidget  state }


-- | Run given action with currently selected song list
withCurrentSongList :: (SongListWidget -> Vimus ()) -> Vimus ()
withCurrentSongList action =  do
  state <- get
  case currentView state of
    Playlist -> action $ playlistWidget state
    Library  -> action $ libraryWidget  state


-- | Run given action with currently selected song, if any
withCurrentSong :: (MPD.Song -> Vimus ()) -> Vimus ()
withCurrentSong action = withCurrentSongList $ \widget ->
  case ListWidget.select widget of
    Just song -> action song
    Nothing   -> return ()
