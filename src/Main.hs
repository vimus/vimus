{-# LANGUAGE FlexibleContexts,FlexibleInstances #-}
module Main where

import UI.Curses hiding (getch, ungetch)
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

status :: (MonadIO m) => m ()
status = do
  s <- withMPD MPD.status
  liftIO $ mvaddstr 12 10 $ show s
  liftIO refresh
  return ()

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

-- experimental commands
runCommand (Just "status")    = status

runCommand (Just c)           = (liftIO $ printStatus $ "unknown command: " ++ c)
runCommand Nothing            = return ()


-- | Print a message to the status line
printStatus :: String -> IO ()
printStatus message = do
  (y, x) <- getyx stdscr
  print_
  move y x
  return ()
  where
    print_ = do
      (y, _) <- getmaxyx stdscr
      mvaddstr (y - 1) 0 message


expandMacro :: Char -> IO ()
expandMacro 'q' = ungetstr ":quit\n"
expandMacro 't' = ungetstr ":toggle\n"
expandMacro 'k'  = ungetstr ":move-up\n"
expandMacro 'j'  = ungetstr ":move-down\n"
expandMacro '\n' = ungetstr ":play_\n"
expandMacro _   = return ()

data ProgramState = ProgramState {
  playlistWidget :: ListWidget MPD.Song
}

class (MonadState ProgramState m, MonadIO m) => MonadVimus m
instance MonadVimus (StateT ProgramState IO)

-- The main event loop
loop :: (MonadVimus m) => m ()
loop = do
  state <- get
  liftIO $ renderListWidget $ playlistWidget state
  c <- liftIO $ getch
  if c == ':'
    then do
      (y, _) <- liftIO $ getmaxyx stdscr
      liftIO $ mvaddstr (y - 1) 0 ":"
      liftIO $ refresh

      input <- liftIO $ readline
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

renderListWidget :: ListWidget a -> IO ()
renderListWidget l = do
  erase
  putList $ choices l
  mvaddstr (position l) 0 $ "*"
  refresh
  return ()
  where
    putList = mapM_ addstrLn
      where
        addstrLn x = addstr $ "  " ++ (render l $ x) ++ "\n"

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

run :: IO ()
run = do
  pl <- createPlaylistWidget
  runStateT loop $ ProgramState pl
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

  curs_set 0

  finally run endwin

  return ()
