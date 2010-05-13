{-# LANGUAGE FlexibleContexts,FlexibleInstances #-}
module Main where

import UI.Curses hiding (wgetch, ungetch, mvaddstr)
import Control.Exception (finally)
--import Control.Monad.Error.Class (throwError)
import System.Exit (exitSuccess)

import qualified Network.MPD as MPD
import Control.Monad.State

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


-- Process a command
runCommand :: (MonadVimus m) => Maybe String -> m ()
runCommand (Just "exit")      = liftIO $ exitSuccess
runCommand (Just "quit")      = liftIO $ exitSuccess
runCommand (Just "next")      = next
runCommand (Just "toggle")    = toggle
runCommand (Just "move-up")   = do
                                  state <- get
                                  put $ state { playlistWidget = moveUp   $ playlistWidget state }
runCommand (Just "move-down") = do
                                  state <- get
                                  put $ state { playlistWidget = moveDown $ playlistWidget state }
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
expandMacro '\n' = ungetstr ":play_\n"
expandMacro _   = return ()

data ProgramState = ProgramState {
  playlistWidget  :: ListWidget MPD.Song
, mainWindow      :: Window
, statusLine      :: Window
}

class (MonadState ProgramState m, MonadIO m) => MonadVimus m
instance MonadVimus (StateT ProgramState IO)

-- The main event loop
loop :: (MonadVimus m) => m ()
loop = do
  state <- get
  let mainwin = mainWindow state
  liftIO $ renderListWidget mainwin $ playlistWidget state
  c <- liftIO $ wgetch mainwin
  if c == ':'
    then do
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
  position  :: Int
, choices   :: [a]
, render    :: a -> String
}

moveUp :: ListWidget a -> ListWidget a
moveUp l = l {position = newPosition}
  where
    newPosition = max 0 (position l - 1)

moveDown :: ListWidget a -> ListWidget a
moveDown l = l {position = newPosition}
  where
    newPosition = min (length (choices l) - 1) (position l + 1)

select :: ListWidget a -> a
select l = choices l !! position l

renderListWidget :: Window -> ListWidget a -> IO ()
renderListWidget win l = do
  werase win
  putList $ choices l
  mvwaddstr win (position l) 0 $ "*"
  wrefresh win
  return ()
  where
    putList = mapM_ addstrLn
      where
        addstrLn x = waddstr win $ "  " ++ (render l $ x) ++ "\n"

------------------------------------------------------------------------
-- playlist widget

songToString :: MPD.Song -> String
songToString s = MPD.sgArtist s ++ " - " ++ MPD.sgTitle s

playlistAll :: IO [MPD.Song]
playlistAll = withMPD $ MPD.playlistInfo Nothing

createPlaylistWidget :: IO (ListWidget MPD.Song)
createPlaylistWidget = do
  songs <- playlistAll
  return $ ListWidget {position = 0, choices = songs, render = songToString}


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
