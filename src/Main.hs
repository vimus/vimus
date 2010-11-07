{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Main where

import UI.Curses hiding (wgetch, ungetch, mvaddstr)
import Control.Exception (finally)
--import Control.Monad.Error.Class (throwError)
import System.Exit (exitSuccess)

import qualified Network.MPD as MPD hiding (withMPD)
import Network.MPD ((=?), Seconds)
import Network.MPD.Core

import Control.Monad.State
import Control.Monad.Error

import Data.Either (rights)
import Data.List
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

import Control.Concurrent

import Text.Printf (printf)

import Prelude hiding (getChar)

import qualified Input
import Macro (expandMacro)

import ListWidget hiding (search, render, select, update)
import qualified ListWidget

import qualified PlaybackState
import PlaybackState (PlaybackState)

import Option (getOptions)
import Util (withMPDEx_)

------------------------------------------------------------------------
-- playlist widget

type SongListWidget = ListWidget MPD.Song

createSongListWidget :: (MonadIO m) => Window -> [MPD.Song] -> m SongListWidget
createSongListWidget window songs = do
  liftIO $ newListWidget render songs window
  where
    render :: MPD.Song -> String
    render song = MPD.sgArtist song ++ " - " ++ MPD.sgAlbum song ++ " - " ++ (show $ MPD.sgTrack song) ++ " - " ++  MPD.sgTitle song


updatePlaylist :: Vimus ()
updatePlaylist = do
  state <- get
  songs <- MPD.getPlaylist
  let newPlaylistWidget = ListWidget.update (playlistWidget state) songs
  put state { playlistWidget = newPlaylistWidget }


updateLibrary :: Vimus ()
updateLibrary = do
  state <- get
  -- it seems that we only get rights here, even with songs that have no
  -- id3tags attached
  songs <- fmap rights $ MPD.listAllInfo ""
  let newWidget = ListWidget.update (libraryWidget state) songs
  put state { libraryWidget = newWidget }


------------------------------------------------------------------------
-- commands

-- | Run given action with currently selected song
withCurrentSong :: (MPD.Song -> Vimus ()) -> Vimus ()
withCurrentSong action = do
  widget <- getCurrentWindow
  let song = ListWidget.select widget
  maybe (return ()) action song

-- | Process a command
runCommand :: Maybe String -> Vimus ()
runCommand (Just "exit")      = liftIO exitSuccess
runCommand (Just "quit")      = liftIO exitSuccess
runCommand (Just "next")      = MPD.next
runCommand (Just "toggle")    = MPD.toggle
runCommand (Just "stop")      = MPD.stop

runCommand (Just "update")    = MPD.update []

runCommand (Just "clear")     = MPD.clear >> updatePlaylist

runCommand (Just "search-next")   = searchNext
runCommand (Just "search-prev")   = searchPrev

runCommand (Just "move-up")       = withCurrentWindow moveUp
runCommand (Just "move-down")     = withCurrentWindow moveDown
runCommand (Just "move-first")    = withCurrentWindow moveFirst
runCommand (Just "move-last")     = withCurrentWindow moveLast
runCommand (Just "scroll-up")     = withCurrentWindow scrollUp
runCommand (Just "scroll-down")   = withCurrentWindow scrollDown

runCommand (Just "scroll-page-up")     = withCurrentWindow scrollPageUp
runCommand (Just "scroll-page-down")   = withCurrentWindow scrollPageDown

runCommand (Just "window-library")       = modify (\s -> s { currentWindow = Library })
runCommand (Just "window-playlist")      = modify (\s -> s { currentWindow = Playlist })
runCommand (Just "window-next")   = modify (\s -> s { currentWindow = invert $ currentWindow s })
                                    where
                                      invert Playlist = Library
                                      invert Library  = Playlist

runCommand (Just "play_")     = withCurrentSong play
                                where
                                  play song = do
                                    case MPD.sgIndex song of
                                      -- song is already on the playlist
                                      (Just i) -> MPD.play (Just i)
                                      -- song is not yet on the playlist
                                      Nothing  -> do
                                                  i_ <- MPD.addId (MPD.sgFilePath song) Nothing
                                                  let i = MPD.ID i_
                                                  MPD.play (Just i)
                                                  updatePlaylist

runCommand (Just "remove")    = withCurrentSong remove
                                where
                                  -- | Remove given song from playlist
                                  remove song = do
                                    case MPD.sgIndex song of
                                      (Just i) -> do MPD.delete i
                                                     updatePlaylist
                                      Nothing  -> return ()

runCommand (Just "add-album") = withCurrentSong action
                                where
                                  -- | Add all songs of given songs album
                                  action song = do
                                    songs <- MPD.find (MPD.Album =? MPD.sgAlbum song)
                                    mapM_ (MPD.add_ . MPD.sgFilePath) songs
                                    updatePlaylist

runCommand (Just "add")       = withCurrentSong add
                                where
                                  -- | Add given song to playlist
                                  add song = do
                                    _ <- MPD.addId (MPD.sgFilePath song) Nothing
                                    updatePlaylist
                                    withCurrentWindow moveDown

-- no command
runCommand (Just c)           = printStatus $ "unknown command: " ++ c
runCommand Nothing            = return ()


-- | Print a message to the status line
printStatus :: String -> Vimus ()
printStatus message = do
  status <- get
  let window = inputLine status
  liftIO $ mvwaddstr window 0 0 message
  liftIO $ wclrtoeol window
  liftIO $ wrefresh window
  return ()

------------------------------------------------------------------------
-- program state

data CurrentWindow = Playlist | Library


-- | Return currently selected song list.
getCurrentWindow :: (MonadState ProgramState m) => m SongListWidget
getCurrentWindow = do
  state <- get
  case currentWindow state of
    Playlist -> return $ playlistWidget state
    Library  -> return $ libraryWidget  state


-- | Modify currently selected song list by applying given function.
withCurrentWindow :: (MonadState ProgramState m) => (SongListWidget -> SongListWidget) -> m ()
withCurrentWindow func = modify $ \state ->
  case currentWindow state of
    Playlist -> state { playlistWidget = func $ playlistWidget state }
    Library  -> state { libraryWidget  = func $ libraryWidget  state }


data ProgramState = ProgramState {
  currentWindow   :: CurrentWindow
, playlistWidget  :: SongListWidget
, libraryWidget   :: SongListWidget
, mainWindow      :: Window
, inputLine       :: Window
, getLastSearchTerm :: String
}


instance MonadMPD (StateT ProgramState MPD) where
  open        = lift open
  close       = lift close
  send        = lift . send
  getPassword = lift getPassword

newtype Vimus a = Vimus {
  runVimus :: StateT ProgramState MPD a
} deriving (Monad, Functor, MonadIO, MonadState ProgramState, MonadError MPDError, MonadMPD)


renderMainWindow :: Vimus ()
renderMainWindow = getCurrentWindow >>= ListWidget.render

------------------------------------------------------------------------
-- The main event loop

inputLoop :: Window -> Chan Notify -> IO ()
inputLoop window chan = do
  forever $ do
    c <- getChar
    case c of
      ':' ->  do
                input <- Input.readline_ window ':'
                notify $ NotifyCommand input
      '/' ->  do
                input <- Input.readline searchPreview window '/'
                maybe (return ()) (notifyAction . search) input
      _   ->  do
                expandMacro getChar Input.ungetstr [c]
  where
    getChar = Input.wgetch window

    notify = writeChan chan
    notifyAction = notify . NotifyAction

    searchPreview term = notifyAction $ do
      -- render preview but do not modify state on each keystroke
      w <- getCurrentWindow
      ListWidget.render $ ListWidget.search (searchPredicate term) w


data Notify = NotifyPlaylistChanged
            | NotifyLibraryChanged
            | NotifyCommand (Maybe String) -- FIXME: get rid of Maybe
            | NotifyAction (Vimus ())

mainLoop :: Chan Notify -> Vimus ()
mainLoop notifyChan = do
  printStatus "type 'q' to exit, read 'src/Macro.hs' for help"
  forever $ do
    notify <- liftIO $ readChan notifyChan
    case notify of
      NotifyPlaylistChanged -> updatePlaylist >> renderMainWindow
      NotifyLibraryChanged  -> updateLibrary >> renderMainWindow
      NotifyCommand c       -> runCommand c `catchError` (printStatus . show) >> renderMainWindow
      NotifyAction action   -> action

------------------------------------------------------------------------
-- search

data SearchOrder = Forward | Backward

search :: String -> Vimus ()
search term = do
  modify $ \state -> state { getLastSearchTerm = term }
  search_ Forward term

searchNext :: Vimus ()
searchNext = do
  state <- get
  search_ Forward $ getLastSearchTerm state

searchPrev :: Vimus ()
searchPrev = do
  state <- get
  search_ Backward $ getLastSearchTerm state

search_ :: SearchOrder -> String -> Vimus ()
search_ order term = do
  withCurrentWindow $ searchMethod order $ searchPredicate term
  where
    searchMethod Forward  = ListWidget.search
    searchMethod Backward = ListWidget.searchBackward

searchPredicate :: String -> MPD.Song -> Bool
searchPredicate "" _ = False
searchPredicate term song =
 or [ match $ MPD.sgArtist song
    , match $ MPD.sgAlbum song
    , match $ MPD.sgTitle song
    , match $ MPD.sgFilePath song
    , match $ MPD.sgGenre song
    , match $ MPD.sgName song
    , match $ MPD.sgComposer song
    , match $ MPD.sgPerformer song
    --sgAux :: [(String, String)]
    ]
  where
    match s = isInfixOf term_ $ map toLower s
    term_ = map toLower term

------------------------------------------------------------------------
-- mpd status

statusThread :: Window -> Window -> PlaybackState -> IO ()
statusThread songWindow playWindow st = do

  putString songWindow song
  putString playWindow playState
  where
    song = fromMaybe "none" $ fmap MPD.sgTitle $ PlaybackState.currentSong st

    playState = stateSymbol ++ " " ++ formatTime current ++ " / " ++ formatTime total
      where
        (current, total) = PlaybackState.elapsedTime st
        stateSymbol = case PlaybackState.playState st of
          MPD.Playing -> "|>"
          MPD.Paused  -> "||"
          MPD.Stopped -> "[]"

    formatTime :: Seconds -> String
    formatTime s = printf "%02d:%02d" minutes seconds
      where
        minutes = s `div` 60
        seconds = s `mod` 60

    putString :: (MonadIO m) => Window -> String -> m ()
    putString window string = liftIO $ do
      mvwaddstr window 0 0 string
      wclrtoeol window
      wrefresh window
      return ()

------------------------------------------------------------------------
-- Program entry point

run :: Maybe String -> Maybe Port -> IO ()
run host port = do
  (sizeY, _)    <- getmaxyx stdscr
  let mainWinCols = sizeY - 3
  mw <- newwin mainWinCols 0 0 0
  songStatusWindow <- newwin 1 0 mainWinCols       0
  playStatusWindow <- newwin 1 0 (mainWinCols + 1) 0
  inputWindow  <- newwin 1 0 (mainWinCols + 2) 0

  pl <- createPlaylistWidget mw
  lw <- createLibraryWidget mw

  forkIO $ withMPD $ PlaybackState.onChange $ statusThread songStatusWindow playStatusWindow

  -- thread for asynchronous updates
  notifyChan <- newChan
  liftIO $ writeChan notifyChan NotifyPlaylistChanged
  liftIO $ writeChan notifyChan NotifyLibraryChanged
  forkIO $ withMPD $ forever $ do
    l <- MPD.idle
    when (MPD.Playlist `elem` l) $ do
      liftIO $ writeChan notifyChan NotifyPlaylistChanged
    when (MPD.Database `elem` l) $ do
      liftIO $ writeChan notifyChan NotifyLibraryChanged

  -- input thread
  forkIO $ inputLoop inputWindow notifyChan

  init_pair 1 green black
  init_pair 2 blue white
  wbkgd mw $ color_pair 2
  wbkgd inputWindow $ color_pair 1
  wrefresh mw
  keypad inputWindow True
  keypad mw True
  wrefresh inputWindow

  withMPD $ runStateT (runVimus $ mainLoop notifyChan) ProgramState {
      currentWindow   = Playlist
    , playlistWidget  = pl
    , libraryWidget   = lw
    , mainWindow      = mw
    , inputLine       = inputWindow
    , getLastSearchTerm = ""
    }
  return ()

  where
    createPlaylistWidget :: Window -> IO SongListWidget
    createPlaylistWidget window = createSongListWidget window []

    createLibraryWidget :: Window -> IO SongListWidget
    createLibraryWidget window = createSongListWidget window []

    withMPD :: (MonadIO m) => MPD.MPD a -> m a
    withMPD action = do
      result <- liftIO $ withMPDEx_ host port action
      case result of
          Left  e -> fail $ show e
          Right r -> return r



main :: IO ()
main = do

  (host, port) <- getOptions

  -- recommended in ncurses manpage
  initscr
  raw
  noecho

  -- suggested  in ncurses manpage
  -- nonl
  intrflush stdscr True
  keypad stdscr True

  -- enable colors
  start_color
  use_default_colors

  curs_set 0

  finally (run host port) endwin
