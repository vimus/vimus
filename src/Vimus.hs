{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Vimus (
  Vimus
, ProgramState (..)
, Action (..)
, Command (..)
, View (..)
, Event (..)
, tabFromList
, sendEvent
, sendEventCurrent
, Handler

, Widget (..)
, WidgetCommand
, WidgetAction
, widgetCommand
, SearchPredicate (..)
, SearchOrder (..)

-- * changing the current view
, nextView
, previousView
, setCurrentView

, withCurrentSong
, withSelected
, withCurrentItem
, withCurrentWidget
, setCurrentWidget
, modifyCurrentWidget
, renderMainWindow
, renderToMainWindow
, addMacro
, setLibraryPath
) where

import Control.Monad.State (liftIO, gets, get, put, modify, StateT, MonadState)
import Control.Monad.Trans (MonadIO)
import Control.Monad

import Data.Default
import Data.Ord (comparing)
import Data.Function (on)

import Network.MPD.Core
import Network.MPD as MPD (LsResult)
import qualified Network.MPD as MPD hiding (withMPD)

import UI.Curses

import ListWidget (ListWidget, Searchable)
import qualified ListWidget

import qualified Macro
import           Macro (Macros)

import           Content
import           Type

-- | Widgets
data Widget = Widget {
    render      :: (MonadIO m) => Window -> m ()
  , title       :: String
  , commands    :: [WidgetCommand]
  , event       :: Event -> Vimus Widget
  , currentItem :: Maybe Content
  , searchItem  :: SearchOrder -> String -> Widget
  , filterItem  :: String -> Widget
}

data SearchOrder = Forward | Backward
data SearchPredicate = Search | Filter

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

type WidgetCommand = (String, WidgetAction)
type WidgetAction  = Vimus (Maybe Widget)

widgetCommand :: String -> WidgetAction -> WidgetCommand
widgetCommand = (,)

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

data ProgramState = ProgramState {
  tabView            :: TabView
, mainWindow         :: Window
, statusLine         :: Window
, tabWindow          :: Window
, getLastSearchTerm  :: String
, programStateMacros :: Macros
, libraryPath        :: Maybe String
}

type Vimus a = StateT ProgramState MPD a


-- | Tab zipper
data TabView = TabView ![Tab] ![Tab]

type Tab = (View, Widget)

data View = Playlist | Library | Browser | SearchResult | Help
  deriving (Eq, Show)


tabName :: Tab -> View
tabName = fst

tabWidget :: Tab -> Widget
tabWidget = snd


tabFromList :: [Tab] -> TabView
tabFromList = TabView []

tabNext :: TabView -> TabView
tabNext (TabView prev next) = case next of
  [this]    -> TabView [] (reverse $ this:prev)
  this:rest -> TabView (this:prev) rest
  _         -> error "No tabs!"

tabPrev :: TabView -> TabView
tabPrev (TabView prev next) = case prev of
  this:rest -> TabView rest (this:next)
  []        -> TabView (tail list) [head list]
                where list = reverse next

currentTab :: TabView -> Tab
currentTab (TabView _ next) = case next of
  this:_ -> this
  []     -> error "No tabs!"

-- Sanity check function, useful if we ever decide to change tabName to String
-- instead of View
hasTab :: TabView -> View -> Bool
hasTab (TabView prev next) v = prev `has` v || next `has` v
  where
    has :: [Tab] -> View -> Bool
    has []     _ = False
    has (x:xs) y = (tabName x == y) || xs `has` y

getTabs :: TabView -> [Tab]
getTabs (TabView prev next) = reverse prev ++ next

selectTab :: View -> TabView -> TabView
selectTab v tv = case tv `hasTab` v of
  True  -> TabView (reverse prev) next
            where (prev, next) = break ((== v) . tabName) (getTabs tv)
  False -> tv


-- | Set path to music library
--
-- This is need, if you want to use %-expansion in commands.
setLibraryPath :: FilePath -> Vimus ()
setLibraryPath p = modify (\state -> state { libraryPath = Just p })

modifyTabs :: (TabView -> TabView) -> Vimus ()
modifyTabs f = modify (\state -> state { tabView = f $ tabView state })

withCurrentTab :: (Tab -> Vimus a) -> Vimus a
withCurrentTab action = do
  state <- get
  action $ currentTab (tabView state)

getCurrentView :: Vimus View
getCurrentView = do
  state <- get
  return (tabName . currentTab $ tabView state)

setCurrentView :: View -> Vimus ()
setCurrentView v = do
  modifyTabs $ selectTab v
  renderTabBar

-- switch to next view
nextView :: Vimus ()
nextView = do
  modifyTabs $ tabNext
  new <- getCurrentView

  -- skip Help
  when (new == Help) nextView

  {-
  -- skip SearchResult, if null
  when (new == SearchResult) $ do
    w <- gets searchResult
    when (ListWidget.null w) nextView
  -}

-- | switch to previous view
previousView :: Vimus ()
previousView = do
  modifyTabs $ tabPrev
  new <- getCurrentView

  -- skip Help
  when (new == Help) previousView

  {-
  -- skip SearchResult, if null
  when (new == SearchResult) $ do
    w <- gets searchResult
    when (ListWidget.null w) previousView
  -}

-- | Run given action with currently selected item, if any
withSelected :: Default b => ListWidget a -> (a -> Vimus b) -> Vimus b
withSelected list action =
  case ListWidget.select list of
    Just item -> action item
    Nothing   -> return def

-- | Run given action with currently selected song, if any
withCurrentSong :: Default a => ListWidget Content -> (MPD.Song -> Vimus a) -> Vimus a
withCurrentSong list action =
  case ListWidget.select list of
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
  let (TabView prev next) = tabView state
  let f (n,w) = (n,) `fmap` action w
  prevs <- mapM f prev
  nexts <- mapM f next

  put state { tabView = TabView prevs nexts }

modifyCurrentWidget :: (Widget -> Vimus Widget) -> Vimus ()
modifyCurrentWidget f = withCurrentWidget f >>= setCurrentWidget

withCurrentWidget :: (Widget -> Vimus b) -> Vimus b
withCurrentWidget action = withCurrentTab $ action . tabWidget

setCurrentWidget :: Widget -> Vimus ()
setCurrentWidget w = do
  state <- get
  case tabView state of
    TabView prev (this:rest) -> put state { tabView = TabView prev ((tabName this, w) : rest) }
    _                        -> error "No tabs!"

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
    mvwaddstr window 0 1 $ "|" ++ show (fst . currentTab $ tabView s) ++ "| " ++ title widget
    wclrtoeol window
    wrefresh window
  return ()
