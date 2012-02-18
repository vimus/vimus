{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Vimus (
  Vimus
, ProgramState (..)
, Action (..)
, Command (..)
, CurrentView (..)

-- * changing the current view
, nextView
, previousView
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
, setLibraryPath
) where

import Control.Monad.State (liftIO, gets, get, put, modify, lift, StateT, MonadState)
import Control.Monad

import Data.Default
import Data.Ord (comparing)
import Data.Function (on)

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

  -- | An action that expects an arbitrary (possibly empty) strings as argument
  --
  -- This can be used to implement variadic actions.
    Action  (String -> Vimus ())

  -- | An action that expects no arguments
  | Action0 (Vimus ())

  -- | An action that expects one argument
  | Action1 (String -> Vimus ())

  -- | An action that expects two argument
  | Action2 (String -> String -> Vimus ())

data Command = Command {
  commandName   :: String
, commandAction :: Action
}

instance Show Command where
  show = commandName

instance Eq Command where
  (==) = (==) `on` commandName

instance Ord Command where
  compare = comparing commandName

-- | Define a macro.
addMacro :: String -- ^ macro
         -> String -- ^ expansion
         -> Vimus ()
addMacro m c = do
  st <- get
  put (st {programStateMacros = Macro.addMacro m c (programStateMacros st)})

data ProgramState = ProgramState {
  currentView         :: CurrentView
, playlistWidget      :: ListWidget Content
, libraryWidget       :: ListWidget Content
, searchResult        :: ListWidget Content
, browserWidget       :: ListWidget Content
, helpWidget          :: ListWidget Command
, mainWindow          :: Window
, statusLine          :: Window
, tabWindow           :: Window
, getLastSearchTerm   :: String
, programStateMacros  :: Macros
, libraryPath         :: Maybe String
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


-- | Set path to music library
--
-- This is need, if you want to use %-expansion in commands.
setLibraryPath :: FilePath -> Vimus ()
setLibraryPath p = modify (\state -> state { libraryPath = Just p })


data CurrentView = Playlist | Library | Browser | SearchResult | Help
  deriving (Eq, Show, Enum, Bounded)

setCurrentView :: CurrentView -> Vimus ()
setCurrentView v = do
  modify (\state -> state { currentView = v })
  renderTabBar

-- switch to next view
nextView :: Vimus ()
nextView = do
  v <- gets currentView
  let new | v == maxBound = minBound
          | otherwise     = succ v
  setCurrentView new

  -- skip Help
  when (new == Help) nextView

  -- skip SearchResult, if null
  when (new == SearchResult) $ do
    w <- gets searchResult
    when (ListWidget.null w) nextView

-- | switch to previous view
previousView :: Vimus ()
previousView = do
  v <- gets currentView
  let new | v == minBound = maxBound
          | otherwise     = pred v
  setCurrentView new

  -- skip Help
  when (new == Help) previousView

  -- skip SearchResult, if null
  when (new == SearchResult) $ do
    w <- gets searchResult
    when (ListWidget.null w) previousView


-- | Modify currently selected list by applying given function.
modifyCurrentList :: (MonadState ProgramState m) => (forall a. ListWidget a -> ListWidget a) -> m ()
modifyCurrentList f = do
  state <- get
  case currentView state of
    Help         -> put state { helpWidget = f $ helpWidget state }
    _            -> modifyCurrentSongList f

modifyCurrentSongList :: (MonadState ProgramState m) => (ListWidget Content -> ListWidget Content) -> m ()
modifyCurrentSongList f = do
  state <- get
  case currentView state of
    Playlist     -> put state { playlistWidget = f $ playlistWidget state }
    Library      -> put state { libraryWidget  = f $ libraryWidget  state }
    SearchResult -> put state { searchResult   = f $ searchResult   state }
    Browser      -> put state { browserWidget  = f $ browserWidget  state }
    Help         -> return ()


-- | Run given action with currently selected list
withCurrentList :: Default a => (forall b. ListWidget b -> Vimus a) -> Vimus a
withCurrentList action =  do
  state <- get
  case currentView state of
    Help         -> action $ helpWidget state
    _            -> withCurrentSongList action

withCurrentSongList :: Default a => (ListWidget Content -> Vimus a) -> Vimus a
withCurrentSongList action = do
  state <- get
  case currentView state of
    Playlist     -> action $ playlistWidget state
    Library      -> action $ libraryWidget  state
    SearchResult -> action $ searchResult   state
    Browser      -> action $ browserWidget  state
    Help         -> return def


-- | Run given action with currently selected item, if any
withCurrentItem :: Default a => (Content -> Vimus a) -> Vimus a
withCurrentItem action = withCurrentSongList $ \widget ->
  case ListWidget.select widget of
    Just item -> action item
    Nothing   -> return def

-- | Run given action with currently selected song, if any
withCurrentSong :: Default a => (MPD.Song -> Vimus a) -> Vimus a
withCurrentSong action = withCurrentItem $ \item ->
  case item of
    Song song -> action song
    _         -> return def

withCurrentWidget :: (forall a. Widget a => a -> Vimus b) -> Vimus b
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
