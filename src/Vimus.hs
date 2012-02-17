{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Vimus (
  Vimus
, ProgramState (..)
, Action (..)
, Command (..)
, CurrentView (..)
, getCurrentView
, setCurrentView
, modifyCurrentList
, modifyCurrentSongList
, withCurrentList
, withCurrentSongList
, withCurrentSong
, withCurrentItem
, renderMainWindow
, renderToMainWindow
, addMacro
) where

import Control.Monad.State (liftIO, gets, get, put, modify, liftM, lift, StateT, MonadState)

import Network.MPD.Core
import qualified Network.MPD as MPD hiding (withMPD)

import UI.Curses

import Widget (Widget)
import qualified Widget

import ListWidget (ListWidget)
import qualified ListWidget

import qualified Macro
import           Macro (Macros)

import Content

-- | Define a command.

data Action =
    Action0 (Vimus ())
  | Action1 (String -> Vimus ())
  | Action2 (String -> String -> Vimus ())

data Command = Command {
  commandName   :: String
, commandAction :: Action
}

instance Show Command where
  show = commandName

-- | Define a macro.
addMacro :: String -- ^ macro
         -> String -- ^ expansion
         -> Vimus ()
addMacro m c = do
  st <- get
  put (st {programStateMacros = Macro.addMacro m c (programStateMacros st)})

data CurrentView = Playlist | Library | Help | SearchResult | Browser
  deriving Show

data ProgramState = ProgramState {
  currentView       :: CurrentView
, playlistWidget    :: ListWidget Content
, libraryWidget     :: ListWidget Content
, searchResult      :: ListWidget Content
, browserWidget     :: ListWidget Content
, helpWidget        :: ListWidget Command
, mainWindow        :: Window
, statusLine        :: Window
, tabWindow         :: Window
, getLastSearchTerm :: String
, programStateMacros :: Macros
}


instance MonadMPD (StateT ProgramState MPD) where
  getVersion  = lift getVersion
  open        = lift open
  close       = lift close
  send        = lift . send
  getHandle   = lift getHandle
  setPassword = lift . setPassword
  getPassword = lift getPassword

type Vimus a = StateT ProgramState MPD a

{-
newtype Vimus a = Vimus {
  runVimus :: StateT ProgramState MPD a
} deriving (Monad, Functor, MonadIO, MonadState ProgramState, MonadError MPDError, MonadMPD)
-}

setCurrentView :: CurrentView -> Vimus ()
setCurrentView v = do
  modify (\state -> state { currentView = v })
  renderTabBar

getCurrentView :: Vimus CurrentView
getCurrentView = currentView `liftM` get

-- | Modify currently selected list by applying given function.
modifyCurrentList :: (MonadState ProgramState m) => (forall a. ListWidget a -> ListWidget a) -> m ()
modifyCurrentList f = do
  state <- get
  case currentView state of
    Playlist -> put state { playlistWidget = f $ playlistWidget state }
    Library  -> put state { libraryWidget  = f $ libraryWidget  state }
    SearchResult -> put state { searchResult = f $ searchResult state }
    Browser  -> put state { browserWidget  = f $ browserWidget  state }
    Help     -> put state { helpWidget = f $ helpWidget state }

modifyCurrentSongList :: (MonadState ProgramState m) => (ListWidget Content -> ListWidget Content) -> m ()
modifyCurrentSongList f = do
  state <- get
  case currentView state of
    Playlist -> put state { playlistWidget = f $ playlistWidget state }
    Library  -> put state { libraryWidget  = f $ libraryWidget  state }
    SearchResult -> put state { searchResult = f $ searchResult state }
    Browser  -> put state { browserWidget  = f $ browserWidget  state }
    Help     -> return ()


-- | Run given action with currently selected list
withCurrentList :: (forall a. ListWidget a -> Vimus ()) -> Vimus ()
withCurrentList action =  do
  state <- get
  case currentView state of
    Playlist     -> action $ playlistWidget state
    Library      -> action $ libraryWidget  state
    SearchResult -> action $ searchResult   state
    Browser      -> action $ browserWidget  state
    Help         -> action $ helpWidget     state

withCurrentSongList :: (ListWidget Content -> Vimus ()) -> Vimus ()
withCurrentSongList action =  do
  state <- get
  case currentView state of
    Playlist     -> action $ playlistWidget state
    Library      -> action $ libraryWidget  state
    SearchResult -> action $ searchResult   state
    Browser      -> action $ browserWidget  state
    Help         -> return ()


-- | Run given action with currently selected item, if any
withCurrentItem :: (Content -> Vimus ()) -> Vimus ()
withCurrentItem action = withCurrentSongList $ \widget ->
  case ListWidget.select widget of
    Just item -> action item
    Nothing   -> return ()

-- | Run given action with currently selected song, if any
withCurrentSong :: (MPD.Song -> Vimus ()) -> Vimus ()
withCurrentSong action = withCurrentItem $ \item ->
  case item of
    Song song -> action song
    _         -> return ()

withCurrentWidget :: (forall a. Widget a => a -> Vimus ()) -> Vimus ()
withCurrentWidget action = do
  state <- get
  case currentView state of
    Playlist     -> action $ playlistWidget state
    Library      -> action $ libraryWidget  state
    SearchResult -> action $ searchResult   state
    Browser      -> action $ browserWidget  state
    Help         -> action $ helpWidget     state


-- | Render currently selected widget to main window
renderMainWindow :: Vimus ()
renderMainWindow = withCurrentWidget renderToMainWindow


-- | Render given widget to main window
renderToMainWindow :: forall a. Widget a => a -> Vimus ()
renderToMainWindow l = do
  window <- gets mainWindow
  Widget.render window l
  renderTabBar

-- | Render the tab bar, called whenever changing states or drawing to main window
renderTabBar :: Vimus ()
renderTabBar = withCurrentWidget $ \widget -> do
  s <- get
  let window = tabWindow s

  liftIO $ do
    mvwaddstr window 0 1 $ "|" ++ show (currentView s) ++ "| " ++ Widget.title widget
    wclrtoeol window
    wrefresh window
  return()
