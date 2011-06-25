{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes #-}
module Vimus
( Vimus
, ProgramState (..)
, CurrentView (..)
, SongListWidget
, getCurrentView
, setCurrentView
, modifyCurrentSongList
, withCurrentSongList
, withCurrentSong
, renderMainWindow
, renderToMainWindow
)
where

import qualified Control.Monad.State as CMS

import Network.MPD.Core
import qualified Network.MPD as MPD hiding (withMPD)

import UI.Curses (Window)

import Widget (Widget, SongListWidget)
import qualified Widget

import TextWidget (TextWidget)

import qualified ListWidget

data CurrentView = Playlist | Library | Help | SearchResult

data ProgramState = ProgramState {
  currentView       :: CurrentView
, playlistWidget    :: SongListWidget
, libraryWidget     :: SongListWidget
, searchResult      :: SongListWidget
, helpWidget        :: TextWidget
, mainWindow        :: Window
, statusLine        :: Window
, getLastSearchTerm :: String
}


instance MonadMPD (CMS.StateT ProgramState MPD) where
  open        = CMS.lift open
  close       = CMS.lift close
  send        = CMS.lift . send
  getPassword = CMS.lift getPassword

type Vimus a = CMS.StateT ProgramState MPD a

{-
newtype Vimus a = Vimus {
  runVimus :: CMS.StateT ProgramState MPD a
} deriving (Monad, Functor, MonadIO, CMS.MonadState ProgramState, MonadError MPDError, MonadMPD)
-}


setCurrentView :: CurrentView -> Vimus ()
setCurrentView v = CMS.modify (\state -> state { currentView = v })

getCurrentView :: Vimus CurrentView
getCurrentView = currentView `CMS.liftM` CMS.get

-- | Modify currently selected song list by applying given function.
modifyCurrentSongList :: (CMS.MonadState ProgramState m) => (SongListWidget -> SongListWidget) -> m ()
modifyCurrentSongList f = do
  state <- CMS.get
  case currentView state of
    Playlist -> CMS.put state { playlistWidget = f $ playlistWidget state }
    Library  -> CMS.put state { libraryWidget  = f $ libraryWidget  state }
    SearchResult -> CMS.put state { searchResult = f $ searchResult state }
    Help     -> return ()


-- | Run given action with currently selected song list
withCurrentSongList :: (SongListWidget -> Vimus ()) -> Vimus ()
withCurrentSongList action =  do
  state <- CMS.get
  case currentView state of
    Playlist -> action $ playlistWidget state
    Library  -> action $ libraryWidget  state
    SearchResult -> action $ searchResult state
    Help     -> return ()


-- | Run given action with currently selected song, if any
withCurrentSong :: (MPD.Song -> Vimus ()) -> Vimus ()
withCurrentSong action = withCurrentSongList $ \widget ->
  case ListWidget.select widget of
    Just song -> action song
    Nothing   -> return ()


withCurrentWidget :: (forall a. Widget a => a -> Vimus ()) -> Vimus ()
withCurrentWidget action = do
  state <- CMS.get
  case currentView state of
    Playlist -> action $ playlistWidget state
    Library  -> action $ libraryWidget  state
    SearchResult -> action $ searchResult state
    Help     -> case state of ProgramState { helpWidget = x} -> action x


-- | Render currently selected widget to main window
renderMainWindow :: Vimus ()
renderMainWindow = withCurrentWidget renderToMainWindow


-- | Render given widget to main window
renderToMainWindow :: forall a. Widget a => a -> Vimus ()
renderToMainWindow l = do
  s <- CMS.get
  Widget.render (mainWindow s) l
