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

type PlaylistWidget = ListWidget MPD.Song

withPlaylistWidget :: (MonadVimus m) => (PlaylistWidget -> PlaylistWidget) -> m ()
withPlaylistWidget f = do
    state <- get
    put $ state { playlistWidget = f $ playlistWidget state }


-- Process a command
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
  put $ state {playlistWidget = l}

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
-- A list widget

data ListWidget a = ListWidget {
  position :: Int
, offset    :: Int
, choices   :: [a]
, render    :: a -> String
}

newListWidget :: ListWidget a
newListWidget = ListWidget {position = 0, offset = 0, choices = [], render = undefined}

moveUp :: ListWidget a -> ListWidget a
moveUp l = l {position = newPosition}
  where
    newPosition = max 0 (position l - 1)

moveDown :: ListWidget a -> ListWidget a
moveDown l = l {position = newPosition}
  where
    newPosition = min (length (choices l) - 1) (position l + 1)

scrollUp :: ListWidget a -> ListWidget a
scrollUp l = l {offset = newOffset}
  where
    listLength      = length $ choices l
    newOffset       = max 0 $ offset l - 1

scrollDown :: ListWidget a -> ListWidget a
scrollDown l = l {offset = newOffset, position = max currentPosition newOffset}
  where
    listLength      = length $ choices l
    currentPosition = position l
    newOffset       = min (listLength - 1) $ offset l + 1

select :: ListWidget a -> a
select l = choices l !! position l

renderListWidget :: Window -> ListWidget a -> IO (ListWidget a)
renderListWidget win l = do

  (sizeY, sizeX) <- getmaxyx win

  let currentPosition = position l
  let currentOffset = offset l
  let offset_  = max currentOffset (currentPosition - (sizeY - 1))
  let offset__ = min offset_ currentPosition

  let list = take sizeY $ drop offset__ $ choices l

  werase win

  let aString = ("  " ++) . render l
  let putLine (y, e) = mvwaddnstr win y 0 (aString e) sizeX
  mapM_ putLine $ zip [0..] list

  let relativePosition = currentPosition - offset__
  mvwaddstr win relativePosition 0 $ "*"

  wrefresh win

  return $ l {offset = offset__}

------------------------------------------------------------------------
-- playlist widget

songToString :: MPD.Song -> String
songToString s = MPD.sgArtist s ++ " - " ++ MPD.sgTitle s

playlistAll :: IO [MPD.Song]
playlistAll = withMPD $ MPD.playlistInfo Nothing

createPlaylistWidget :: IO PlaylistWidget
createPlaylistWidget = do
  songs <- playlistAll
  return $ newListWidget {choices = songs, render = songToString}


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
  pl <- createPlaylistWidget
  (mw, sw) <- createWindows

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
