{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Vimus (
  Vimus
, ProgramState (..)
, Action (..)
, Command (..)
, TabName (..)
, CloseMode (..)
, Tab (..)
, Event (..)
, sendEvent
, sendEventCurrent
, Handler

, Widget (..)
, SearchOrder (..)

-- * changing the current view
, nextView
, previousView
, setCurrentView
, addTab
, closeTab

, withCurrentSong
, withSelected
, withCurrentItem
, withCurrentWidget
, withCurrentTab
, setCurrentWidget
, modifyCurrentWidget
, renderMainWindow
, renderToMainWindow
, addMacro
, setLibraryPath
) where

import           Prelude hiding (mapM)
import           Data.Functor
import           Data.Traversable (mapM)

import           Control.Monad.State.Strict (liftIO, gets, get, put, modify, StateT, MonadState)
import           Control.Monad.Trans (MonadIO)

import           Data.Default
import           Data.Ord (comparing)
import           Data.Function (on)

import           Network.MPD.Core
import           Network.MPD as MPD (LsResult)
import qualified Network.MPD as MPD hiding (withMPD)

import           UI.Curses

import           ListWidget (ListWidget, Renderable)
import qualified ListWidget

import qualified Macro
import           Macro (Macros)

import           Content
import           Type ()

import           Tab (Tab(..), TabName(..), CloseMode(..))
import qualified Tab

-- | Widgets
data Widget = Widget {
    render      :: !((MonadIO m) => Window -> m ())
  , title       :: !String
  , event       :: !(Event -> Vimus Widget)
  , currentItem :: !(Maybe Content)
  , searchItem  :: !(SearchOrder -> String -> Widget)
  , filterItem  :: !(String -> Widget)
}

data SearchOrder = Forward | Backward

-- | Events
data Event =
    EvCurrentSongChanged (Maybe MPD.Song)
  | EvPlaylistChanged [MPD.Song]
  | EvLibraryChanged [LsResult]
  | EvResize (Int, Int)
  | EvMoveUp
  | EvMoveDown
  | EvMoveIn
  | EvMoveOut
  | EvMoveFirst
  | EvMoveLast
  | EvScrollUp
  | EvScrollDown
  | EvScrollPageUp
  | EvScrollPageDown
  | EvRemove

-- | Send an event to all widgets.
sendEvent :: Event -> Vimus ()
sendEvent e = withAllWidgets (flip event e)

-- | Send an event to current widgets.
sendEventCurrent :: Event -> Vimus ()
sendEventCurrent e = modifyCurrentWidget (flip event e)

type Handler a = Event -> a -> Vimus (Maybe a)

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

  -- | An action that expects two arguments
  | Action2 (String -> String -> Vimus ())

  -- | An action that expects three arguments
  | Action3 (String -> String -> String -> Vimus ())

data Command = Command {
  commandName   :: String
, commandAction :: Action
}

instance Searchable Command where
  searchTags item = [commandName item]

instance Renderable Command where
  renderItem = commandName

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

type Tabs = Tab.Tabs Widget

data ProgramState = ProgramState {
  tabView            :: Tabs
, mainWindow         :: Window
, statusLine         :: Window
, tabWindow          :: Window
, getLastSearchTerm  :: String
, programStateMacros :: Macros
, libraryPath        :: Maybe String
}

type Vimus a = StateT ProgramState MPD a


addTab :: TabName -> Widget -> CloseMode -> Vimus ()
addTab name widget mode = modify (\st -> st {tabView = Tab.insert tab (tabView st)})
  where tab = Tab name widget mode

-- | Close current tab if possible, return True on success.
closeTab :: Vimus Bool
closeTab = do
  st <- get
  maybe (return False) (\tabs -> put st {tabView = tabs} >> return True) (Tab.close (tabView st))

-- | Set path to music library
--
-- This is need, if you want to use %-expansion in commands.
setLibraryPath :: FilePath -> Vimus ()
setLibraryPath p = modify (\state -> state { libraryPath = Just p })

modifyTabs :: (Tabs -> Tabs) -> Vimus ()
modifyTabs f = modify (\state -> state { tabView = f $ tabView state })

withCurrentTab :: (Tab Widget -> Vimus a) -> Vimus a
withCurrentTab action = do
  state <- get
  action $ Tab.current (tabView state)

setCurrentView :: TabName -> Vimus ()
setCurrentView name = do
  modifyTabs $ Tab.select ((== name) . tabName)
  renderTabBar

-- switch to next view
nextView :: Vimus ()
nextView = modifyTabs $ Tab.next

-- | switch to previous view
previousView :: Vimus ()
previousView = modifyTabs $ Tab.previous

-- | Run given action with currently selected item, if any
withSelected :: Default b => ListWidget a -> (a -> Vimus b) -> Vimus b
withSelected list action =
  case ListWidget.select list of
    Just item -> action item
    Nothing   -> return def

-- | Run given action with currently selected song, if any
withCurrentSong :: Default a => (MPD.Song -> Vimus a) -> Vimus a
withCurrentSong action = withCurrentWidget $ \widget ->
  case currentItem widget of
    Just (Song song) -> action song
    _                -> return def

-- | Run given action with currently selected item, if any
withCurrentItem :: Default a => (Content -> Vimus a) -> Vimus a
withCurrentItem action = withCurrentWidget $ \widget ->
  case currentItem widget of
    Just item -> action item
    Nothing   -> return def

-- | Perform an action on all widgets
withAllWidgets :: (Widget -> Vimus Widget) -> Vimus ()
withAllWidgets action = do
  state <- get
  tabs <- mapM action (tabView state)
  put state {tabView = tabs}

modifyCurrentWidget :: (Widget -> Vimus Widget) -> Vimus ()
modifyCurrentWidget f = withCurrentWidget f >>= setCurrentWidget

withCurrentWidget :: (Widget -> Vimus b) -> Vimus b
withCurrentWidget action = withCurrentTab $ action . tabContent

setCurrentWidget :: Widget -> Vimus ()
setCurrentWidget w = modify (\st -> st {tabView = Tab.modify (w <$) (tabView st)})

-- | Render currently selected widget to main window
renderMainWindow :: Vimus ()
renderMainWindow = withCurrentWidget renderToMainWindow


-- | Render given widget to main window
renderToMainWindow :: Widget -> Vimus ()
renderToMainWindow l = do
  window <- gets mainWindow
  render l window
  renderTabBar

-- | Render the tab bar, called whenever changing states or drawing to main window
renderTabBar :: Vimus ()
renderTabBar = withCurrentWidget $ \widget -> do
  s <- get
  let window = tabWindow s

  liftIO $ do
    mvwaddstr window 0 1 $ "|" ++ show (tabName . Tab.current $ tabView s) ++ "| " ++ title widget
    wclrtoeol window
    wrefresh window
  return ()
