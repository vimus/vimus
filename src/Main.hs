module Main where

import UI.Curses hiding (getch, ungetch)
import qualified UI.Curses as Curses
import qualified Key
import Control.Exception (finally)
--import Control.Monad.Error.Class (throwError)
import System.Exit (exitSuccess)

import Foreign (unsafePerformIO)
import Data.IORef

import qualified Network.MPD as MPD

withMPD :: MPD.MPD a -> IO a
withMPD action = do
  result <- MPD.withMPD action
  case result of
                                -- FIXME do we want to use
      Left  e -> fail $ show e  -- throwError e
                                -- here?
      Right r -> return r

next :: IO ()
next = withMPD MPD.next

toggle :: IO ()
toggle = withMPD MPD.toggle

status :: IO ()
status = do
  s <- withMPD MPD.status
  mvaddstr 12 10 $ show s
  refresh
  return ()

-- Process a command
runCommand :: ProgramState -> Maybe String -> IO ProgramState
runCommand _     (Just "exit")      = exitSuccess
runCommand _     (Just "quit")      = exitSuccess
runCommand state (Just "next")      = next >> return state
runCommand state (Just "toggle")    = toggle >> return state
runCommand state (Just "move-up")   = return $ state { playlistWidget = moveUp   $ playlistWidget state }
runCommand state (Just "move-down") = return $ state { playlistWidget = moveDown $ playlistWidget state }
runCommand state (Just "play_")     = do
                                        let song = select $ playlistWidget state
                                        withMPD $ MPD.play $ MPD.sgIndex song
                                        return state

-- experimental commands
runCommand state (Just "status")    = status >> return state

runCommand state (Just c)           = (printStatus $ "unknown command: " ++ c) >> return state
runCommand state Nothing            = return state


-- | Print a message to the status line
printStatus :: String -> IO ()
printStatus message = do
  (y, x) <- getyx stdscr
  print
  move y x
  return ()
  where
    print = do
      (y, _) <- getmaxyx stdscr
      mvaddstr (y - 1) 0 message



-- Ncurses uses a bounded FIFO for ungetch, so we can not use it to put
-- arbitrary-length strings back into the queue.  For now we use the
-- unsafePerformIO hack to work around this.
inputQueue :: IORef String
{-# NOINLINE inputQueue #-}
inputQueue = unsafePerformIO (newIORef "")

-- | Push given string into input queue.
ungetstr :: String -> IO ()
ungetstr s = do
  old <- readIORef inputQueue
  writeIORef inputQueue $ s ++ old


getch :: IO Char
getch = do
  queue <- readIORef inputQueue
  getChar_ queue
  where
    getChar_ []     = Curses.getch
    getChar_ (x:xs) = do
      writeIORef inputQueue xs
      return x

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

-- The main event loop
run :: ProgramState -> IO ()
run state = do
  renderListWidget $ playlistWidget state
  c <- getch
  newState <- if c == ':'
                then do
                  (y, _) <- getmaxyx stdscr
                  mvaddstr (y - 1) 0 ":"
                  refresh

                  input <- readline
                  runCommand state input
                else do
                  expandMacro c >> return state
  run newState


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

createPlaylistWidget = do
  songs <- playlistAll
  return $ ListWidget {position = 0, choices = songs, render = songToString}


------------------------------------------------------------------------
-- Program entry point

main = do
  win <- initscr
  keypad stdscr True
  curs_set 0
  noecho

  pl <- createPlaylistWidget
  finally (run $ ProgramState pl)endwin

  return ()


------------------------------------------------------------------------
-- | Read a line of input
readline :: IO (Maybe String)
readline = do
  echo
  cursorBackup <- curs_set 1
  clrtoeol
  input <- readline_ ""
  _ <- curs_set cursorBackup
  noecho
  return $ fmap reverse input
  where
    readline_ str = do
      c <- getch

      let continue | accept c          = return $ Just str
                   | c == keyBackspace = backspace str
                   | otherwise         = readline_ $ c : str
      continue

      where
        accept c = c == '\n' || c == keyF 1 || c == keyEnter

        backspace [] = return Nothing
        backspace (_:str)= do
          delch
          readline_ str
