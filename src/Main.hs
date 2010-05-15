{-# LANGUAGE FlexibleContexts,FlexibleInstances #-}
module Main where

import UI.Curses hiding (wgetch, ungetch, mvaddstr)
import Control.Exception (finally)
--import Control.Monad.Error.Class (throwError)
import System.Exit (exitSuccess)

import qualified Network.MPD as MPD
import Control.Monad.State

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

------------------------------------------------------------------------
-- playlist widget

type PlaylistWidget = ListWidget MPD.Song

playlistAll :: IO [MPD.Song]
playlistAll = withMPD $ MPD.playlistInfo Nothing

createPlaylistWidget :: Window -> IO PlaylistWidget
createPlaylistWidget window = do
  (sizeY, _) <- getmaxyx window
  songs <- playlistAll
  return $ newListWidget songToString songs sizeY
  where
    songToString :: MPD.Song -> String
    songToString s = MPD.sgArtist s ++ " - " ++ MPD.sgTitle s

withPlaylistWidget :: (MonadVimus m) => (PlaylistWidget -> PlaylistWidget) -> m ()
withPlaylistWidget f = do
    state <- get
    put state {playlistWidget = f $ playlistWidget state}


-- | Process a command
runCommand :: (MonadVimus m) => Maybe String -> m ()
runCommand (Just "exit")      = liftIO $ exitSuccess
runCommand (Just "quit")      = liftIO $ exitSuccess
runCommand (Just "next")      = next
runCommand (Just "toggle")    = toggle
runCommand (Just "move-up")       = withPlaylistWidget moveUp
runCommand (Just "move-down")     = withPlaylistWidget moveDown
runCommand (Just "scroll-up")     = withPlaylistWidget scrollUp
runCommand (Just "scroll-down")   = withPlaylistWidget scrollDown
runCommand (Just "play_")     = do
                                  state <- get
                                  let song = select $ playlistWidget state
                                  withMPD $ MPD.play $ MPD.sgIndex song
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
expandMacro '\n' = ungetstr ":play_\n"
expandMacro _   = return ()

data ProgramState = ProgramState {
  playlistWidget  :: PlaylistWidget
, mainWindow      :: Window
, statusLine      :: Window
}

class (MonadState ProgramState m, MonadIO m) => MonadVimus m
instance MonadVimus (StateT ProgramState IO)

renderMainWindow :: (MonadVimus m) => m ()
renderMainWindow = do
  state <- get
  let mainwin = mainWindow state
  l <- liftIO $ renderListWidget mainwin $ playlistWidget state
  put state {playlistWidget = l}

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

  init_pair 1 green black
  init_pair 2 blue white
  wbkgd mw $ color_pair 2
  wbkgd sw $ color_pair 1
  wrefresh mw
  keypad sw True
  wrefresh sw

  runStateT loop $ ProgramState pl mw sw
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
