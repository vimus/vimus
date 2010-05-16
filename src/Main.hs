{-# LANGUAGE FlexibleContexts,FlexibleInstances #-}
module Main where

import UI.Curses hiding (wgetch, ungetch, mvaddstr)
import Control.Exception (finally)
--import Control.Monad.Error.Class (throwError)
import System.Exit (exitSuccess)

import qualified Network.MPD as MPD
import Control.Monad.State
import Data.Either (rights)

import Prelude hiding (getChar)

import Input

import ListWidget

withMPD :: (MonadIO m) => MPD.MPD a -> m a
withMPD action = do
  result <- liftIO $ MPD.withMPD action
  case result of
                                -- FIXME do we want to use
      Left  e -> fail $ show e  -- throwError e
                                -- here?
      Right r -> return r

next :: (MonadIO m) => m ()
next = withMPD MPD.next

toggle :: (MonadIO m) => m ()
toggle = withMPD MPD.toggle

clear :: (MonadIO m) => m ()
clear = withMPD MPD.clear

------------------------------------------------------------------------
-- playlist widget

type SongListWidget = ListWidget MPD.Song

createSongListWidget :: Window -> [MPD.Song] -> IO SongListWidget
createSongListWidget window songs = do
  newListWidget songToString songs window
  where
    songToString :: MPD.Song -> String
    songToString s = MPD.sgArtist s ++ " - " ++ MPD.sgTitle s


playlistAll :: IO [MPD.Song]
playlistAll = withMPD $ MPD.playlistInfo Nothing

createPlaylistWidget :: Window -> IO SongListWidget
createPlaylistWidget window = createSongListWidget window =<< playlistAll

updatePlaylist :: (MonadVimus m) => m ()
updatePlaylist = do
  state <- get
  pl <- liftIO $ createPlaylistWidget $ mainWindow state
  put state { playlistWidget = pl }

------------------------------------------------------------------------
-- library widget

libraryAll :: IO [MPD.Song]
libraryAll = fmap rights $ withMPD $ MPD.listAllInfo ""

createLibraryWidget :: Window -> IO SongListWidget
createLibraryWidget window = createSongListWidget window =<< libraryAll

------------------------------------------------------------------------
-- commands

-- | Process a command
runCommand :: (MonadVimus m) => Maybe String -> m ()
runCommand (Just "exit")      = liftIO $ exitSuccess
runCommand (Just "quit")      = liftIO $ exitSuccess
runCommand (Just "next")      = next
runCommand (Just "toggle")    = toggle

runCommand (Just "clear")     = clear >> updatePlaylist

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

runCommand (Just "play_")     = withCurrentWindow_ play
                                where
                                  play widget = do
                                    let song = select widget
                                    case song of
                                      Just s  -> playSong s
                                      Nothing -> return ()
                                  playSong s = do
                                    let index = MPD.sgIndex s
                                    case index of
                                      (Just _) -> withMPD $ MPD.play index
                                      Nothing  -> do
                                                    i <- withMPD $ MPD.addId (MPD.sgFilePath s) Nothing
                                                    withMPD $ MPD.play $ Just $ MPD.ID i
                                                    return ()
-- no command
runCommand (Just c)           = printStatus $ "unknown command: " ++ c
runCommand Nothing            = return ()


-- | Print a message to the status line
printStatus :: (MonadVimus m) => String -> m ()
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
expandMacro _   = return ()


------------------------------------------------------------------------
-- program state

data CurrentWindow = Playlist | Library

withCurrentWindow_ :: (MonadVimus m) => (SongListWidget -> IO ()) -> m ()
withCurrentWindow_ f = do
  state <- get
  liftIO $ case currentWindow state of
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

class (MonadState ProgramState m, MonadIO m) => MonadVimus m
instance MonadVimus (StateT ProgramState IO)

renderMainWindow :: (MonadVimus m) => m ()
renderMainWindow = withCurrentWindow_ renderListWidget

getChar :: (MonadVimus m) => m Char
getChar = do
  state <- get
  let mainwin = mainWindow state
  liftIO $ wgetch mainwin


-- The main event loop
loop :: (MonadVimus m) => m ()
loop = do
  renderMainWindow
  c <- getChar
  if c == ':'
    then do
      state <- get
      let window = statusLine state
      liftIO $ mvwaddstr window 0 0 ":"
      liftIO $ wrefresh window

      input <- liftIO $ readline window
      runCommand input
    else do
      liftIO $ expandMacro c
  loop

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

  runStateT loop $ ProgramState {
      currentWindow   = Playlist
    , playlistWidget  = pl
    , libraryWidget   = lw
    , mainWindow      = mw
    , statusLine      = sw
    }
  return ()

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

  return ()
