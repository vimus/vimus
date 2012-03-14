{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Vimus (
  Vimus
, runVimus
, mainWindow
, libraryPath
, getLastSearchTerm

-- * macros
, addMacro
, getMacros

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

, printMessage
, printError
, logMessages

-- * tabs
, previousTab
, nextTab
, selectTab
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
, renderTabBar
, setLibraryPath
) where

import           Prelude hiding (mapM)
import           Data.Functor
import           Data.Traversable (mapM)
import           Data.Foldable (forM_)
import           Control.Monad (when)

import           Control.Monad.State.Strict (liftIO, gets, get, put, modify, evalStateT, StateT, MonadState)
import           Control.Monad.Trans (MonadIO)

import           Data.Default
import           Data.Ord (comparing)
import           Data.Function (on)

import           System.Time (getClockTime, toCalendarTime, formatCalendarTime)
import           System.Locale (defaultTimeLocale)

import           Network.MPD.Core
import           Network.MPD as MPD (LsResult)
import qualified Network.MPD as MPD hiding (withMPD)

import           UI.Curses hiding (mvwchgat)

import           ListWidget (ListWidget, Renderable)
import qualified ListWidget

import qualified Macro
import           Macro (Macros)

import           Content
import           Type ()

import           Tab (Tab(..), TabName(..), CloseMode(..))
import qualified Tab
import           WindowLayout (WindowColor(..), mvwchgat)

import          Control.Monad.Error.Class

-- | Widgets
data Widget = Widget {
    render      :: !((MonadIO m) => Window -> m ())
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
  | EvLogMessage      -- ^ emitted when a message is added to the log

-- | Send an event to all widgets.
sendEvent :: Event -> Vimus ()
sendEvent e = withAllWidgets (flip event e)

-- | Send an event to current widget.
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


-- * commands
data Command = Command {
  commandName   :: String
, commandAction :: Action
}

instance Eq Command where
  (==) = (==) `on` commandName

instance Ord Command where
  compare = comparing commandName

instance Searchable Command where
  searchTags item = [commandName item]

instance Renderable Command where
  renderItem = commandName


-- * log messages
newtype LogMessage = LogMessage String

instance Searchable LogMessage where
  searchTags (LogMessage m) = return m

instance Renderable LogMessage where
  renderItem (LogMessage m) = m


--- * the vimus monad
type Tabs = Tab.Tabs Widget

data ProgramState = ProgramState {
  tabView            :: Tabs
, mainWindow         :: Window
, statusLine         :: Window
, tabWindow          :: Window
, getLastSearchTerm  :: String
, programStateMacros :: Macros
, libraryPath        :: Maybe String
, logMessages        :: [LogMessage]
}

newtype Vimus a = Vimus (StateT ProgramState MPD a)
  deriving (Functor, Monad, MonadIO, MonadState ProgramState, MonadError MPDError, MonadMPD)

runVimus :: Tabs -> Window -> Window -> Window -> Vimus a -> MPD a
runVimus tabs mw statusWindow tw (Vimus action) = evalStateT action st
  where st = ProgramState { tabView            = tabs
                          , mainWindow         = mw
                          , statusLine         = statusWindow
                          , tabWindow          = tw
                          , getLastSearchTerm  = def
                          , programStateMacros = def
                          , libraryPath        = def
                          , logMessages        = def
                          }

-- * macros

-- | Define a macro.
addMacro :: String -- ^ macro
         -> String -- ^ expansion
         -> Vimus ()
addMacro m c = do
  st <- get
  put (st {programStateMacros = Macro.addMacro m c (programStateMacros st)})

getMacros :: Vimus Macros
getMacros = gets programStateMacros


-- | Print an error message.
printError :: String -> Vimus ()
printError message = do
  t <- formatCalendarTime defaultTimeLocale "%H:%M:%S - " <$> liftIO (getClockTime >>= toCalendarTime)
  modify $ \st -> st {logMessages = LogMessage (t ++ message) : logMessages st}
  window <- gets statusLine
  liftIO $ do
    werase window
    mvwaddstr window 0 0 message
    mvwchgat window 0 0 (-1) [] ErrorColor
    wrefresh window
    return ()
  sendEvent EvLogMessage

-- | Print a message.
printMessage :: String -> Vimus ()
printMessage message = do
  window <- gets statusLine
  liftIO $ do
    werase window
    mvwaddstr window 0 0 message
    wrefresh window
    return ()


addTab :: TabName -> Widget -> CloseMode -> Vimus ()
addTab name widget mode = do
  modify (\st -> st {tabView = Tab.insert tab (tabView st)})
  renderTabBar
  where
    tab = Tab name widget mode

-- | Close current tab if possible, return True on success.
closeTab :: Vimus Bool
closeTab = do
  st <- get
  case Tab.close (tabView st) of
    Just tabs -> do
      put st {tabView = tabs}
      renderTabBar
      return True
    Nothing -> return False

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

-- | Set focus to next tab with given name.
selectTab :: TabName -> Vimus ()
selectTab name = do
  modifyTabs $ Tab.select ((== name) . tabName)
  renderTabBar

-- | Set focus to next tab.
nextTab :: Vimus ()
nextTab = do
  modifyTabs $ Tab.next
  renderTabBar

-- | Set focus to previous tab.
previousTab :: Vimus ()
previousTab = do
  modifyTabs $ Tab.previous
  renderTabBar


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

-- |
-- Render the tab bar.
--
-- Needs to be called when ever the current tab changes.
renderTabBar :: Vimus ()
renderTabBar = do

  window <- gets tabWindow
  (pre, c, suf) <- Tab.preCurSuf <$> gets tabView

  let renderTab t = waddstr window $ " " ++ (show $ tabName t) ++ " "

  liftIO $ do
    werase window

    forM_ pre $ \tab -> do
      waddstr window "|"
      renderTab tab

    -- do not draw current tab if it is AutoClose
    when (not $ Tab.isAutoClose c) $ do
      waddstr window "|"
      wattr_on window [Bold]
      renderTab c
      wattr_off window [Bold]
      return ()

    waddstr window "|"

    forM_ suf $ \tab -> do
      renderTab tab
      waddstr window "|"

    wrefresh window
  return ()
