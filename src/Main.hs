module Main where

import UI.Curses hiding (getch, ungetch)
import Control.Exception (finally)
--import Control.Monad.Error.Class (throwError)
import System.Exit (exitSuccess)

import qualified Network.MPD as MPD

import Input

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

-- The main event loop
loop :: ProgramState -> IO ()
loop state = do
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
  loop newState


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
  loop $ ProgramState pl

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
