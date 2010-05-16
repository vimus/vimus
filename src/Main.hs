{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Main where

import UI.Curses hiding (wgetch, ungetch, mvaddstr)
import Control.Exception (finally)
--import Control.Monad.Error.Class (throwError)
import System.Exit (exitSuccess)

import qualified Network.MPD as MPD
import Network.MPD.Core

import Control.Monad.State
import Control.Monad.Error

import Data.Either (rights)
import Data.List

import Prelude hiding (getChar)

import Input

import ListWidget hiding (search)
import qualified ListWidget

------------------------------------------------------------------------
-- playlist widget

type SongListWidget = ListWidget MPD.Song

createSongListWidget :: (MonadIO m) => Window -> [MPD.Song] -> m SongListWidget
createSongListWidget window songs = do
  liftIO $ newListWidget songToString songs window
  where
    songToString :: MPD.Song -> String
    songToString s = MPD.sgArtist s ++ " - " ++ MPD.sgTitle s

updatePlaylist :: Vimus ()
updatePlaylist = do
  state <- get
  songs <- MPD.getPlaylist
  let newPlaylistWidget = update (playlistWidget state) songs
  put state { playlistWidget = newPlaylistWidget }

------------------------------------------------------------------------
-- commands

-- | Run given action with currently selected song
withCurrentSong :: (MPD.Song -> Vimus ()) -> Vimus ()
withCurrentSong action = withCurrentWindow_ $
  \widget -> do
    let song = select widget
    maybe (return ()) action song

-- | Process a command
runCommand :: Maybe String -> Vimus ()
runCommand (Just "exit")      = liftIO $ exitSuccess
runCommand (Just "quit")      = liftIO $ exitSuccess
runCommand (Just "next")      = MPD.next
runCommand (Just "toggle")    = MPD.toggle

runCommand (Just "clear")     = MPD.clear >> updatePlaylist

runCommand (Just "move-up")       = withCurrentWindow moveUp
runCommand (Just "move-down")     = withCurrentWindow moveDown
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
  let window = statusLine status
  liftIO $ mvwaddstr (statusLine status) 0 0 message
  liftIO $ wrefresh window
  return ()


expandMacro :: Char -> IO ()
expandMacro 'q' = ungetstr ":quit\n"
expandMacro 't' = ungetstr ":toggle\n"
expandMacro 'k'  = ungetstr ":move-up\n"
expandMacro 'j'  = ungetstr ":move-down\n"
expandMacro '\25'  = ungetstr ":scroll-up\n"
expandMacro '\5'  = ungetstr ":scroll-down\n"
expandMacro '\2'  = ungetstr ":scroll-page-up\n"
expandMacro '\6'  = ungetstr ":scroll-page-down\n"
expandMacro '\14'  = ungetstr ":window-next\n"
expandMacro '1'    = ungetstr ":window-playlist\n"
expandMacro '2'    = ungetstr ":window-library\n"
expandMacro '\n' = ungetstr ":play_\n"
expandMacro 'd'     = ungetstr ":remove\n"
expandMacro 'a'     = ungetstr ":add\n"
expandMacro _   = return ()


------------------------------------------------------------------------
-- program state

data CurrentWindow = Playlist | Library

withCurrentWindow_ :: (SongListWidget -> Vimus ()) -> Vimus ()
withCurrentWindow_ f = do
  state <- get
  case currentWindow state of
    Playlist -> f $ playlistWidget state
    Library  -> f $ libraryWidget  state

withCurrentWindow :: (MonadState ProgramState m) => (SongListWidget -> SongListWidget) -> m ()
withCurrentWindow f = modify $ withCurrentWindow'
  where
    withCurrentWindow' state =
      case currentWindow state of
        Playlist -> state { playlistWidget = f $ playlistWidget state }
        Library  -> state { libraryWidget  = f $ libraryWidget  state }


data ProgramState = ProgramState {
  currentWindow   :: CurrentWindow
, playlistWidget  :: SongListWidget
, libraryWidget   :: SongListWidget
, mainWindow      :: Window
, statusLine      :: Window
}


instance MonadMPD (StateT ProgramState MPD) where
  open        = lift $ open
  close       = lift $ close
  send        =  lift . send
  getPassword = lift $ getPassword

newtype Vimus a = Vimus {
  runVimus :: StateT ProgramState MPD a
} deriving (Monad, Functor, MonadIO, MonadState ProgramState, MonadError MPDError, MonadMPD)


renderMainWindow :: Vimus ()
renderMainWindow = withCurrentWindow_ $ liftIO . renderListWidget

------------------------------------------------------------------------
-- The main event loop

getChar :: Vimus Char
getChar = do
  state <- get
  let mainwin = mainWindow state
  liftIO $ wgetch mainwin

getInput :: String -> Vimus (Maybe String)
getInput prompt = do
  state <- get
  let window = statusLine state
  liftIO $ mvwaddstr window 0 0 prompt
  liftIO $ wrefresh window
  liftIO $ readline window

loop :: Vimus ()
loop = do
  renderMainWindow
  c <- getChar
  case c of
    ':' ->  do
              input <- getInput ":"
              runCommand input `catchError` (\e -> printStatus $ show e)
    '/' ->  do
              input <- getInput "/"
              maybe (return ()) search input
    _   ->  do
              liftIO $ expandMacro c
  loop

search :: String -> Vimus ()
search term = do
  withCurrentWindow $ ListWidget.search predicate
  where
    predicate :: MPD.Song -> Bool
    predicate song = or [
          match $ MPD.sgArtist song
        , match $ MPD.sgAlbum song
        , match $ MPD.sgTitle song
        , match $ MPD.sgFilePath song
        , match $ MPD.sgGenre song
        , match $ MPD.sgName song
        , match $ MPD.sgComposer song
        , match $ MPD.sgPerformer song
        --sgAux :: [(String, String)]
        ]
    match = isPrefixOf term

------------------------------------------------------------------------
-- Program entry point

createWindows :: IO (Window, Window)
createWindows = do
  (y, _)    <- getmaxyx stdscr
  let mainWinCols = y - 1
  mainwin   <- newwin mainWinCols 0 0 0
  status <- newwin 0 0 mainWinCols 0
  return (mainwin, status)

run :: IO ()
run = do
  (mw, sw) <- createWindows

  pl <- createPlaylistWidget mw
  lw <- createLibraryWidget mw

  init_pair 1 green black
  init_pair 2 blue white
  wbkgd mw $ color_pair 2
  wbkgd sw $ color_pair 1
  wrefresh mw
  keypad sw True
  wrefresh sw

  withMPD $ runStateT (runVimus loop) $ ProgramState {
      currentWindow   = Playlist
    , playlistWidget  = pl
    , libraryWidget   = lw
    , mainWindow      = mw
    , statusLine      = sw
    }
  return ()

  where
    playlistAll :: IO [MPD.Song]
    playlistAll = withMPD $ MPD.getPlaylist

    createPlaylistWidget :: Window -> IO SongListWidget
    createPlaylistWidget window = createSongListWidget window =<< playlistAll

    libraryAll :: IO [MPD.Song]
    libraryAll = fmap rights $ withMPD $ MPD.listAllInfo ""

    createLibraryWidget :: Window -> IO SongListWidget
    createLibraryWidget window = createSongListWidget window =<< libraryAll

    withMPD :: (MonadIO m) => MPD.MPD a -> m a
    withMPD action = do
      result <- liftIO $ MPD.withMPD action
      case result of
          Left  e -> fail $ show e
          Right r -> return r



main :: IO ()
main = do

  -- recommended in ncurses manpage
  initscr
  cbreak
  noecho

  -- suggested  in ncurses manpage
  -- nonl
  intrflush stdscr True
  keypad stdscr True

  -- enable colors
  start_color
  use_default_colors

  curs_set 0

  finally run endwin
