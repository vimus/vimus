module Main where

import UI.Curses
import qualified Key
import Control.Exception (finally)
--import Control.Monad.Error.Class (throwError)

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
runCommand :: Maybe String -> IO ()
runCommand (Just "exit")    = return ()
runCommand (Just "quit")    = return ()
runCommand (Just "next")    = next  >> run
runCommand (Just "toggle")  = toggle >> run

-- experimental commands
runCommand (Just "status")  = status >> run

runCommand _                = run

-- The main event loop
run = do
  c <- getch
  if c == ':'
      then do
        (y, _) <- getmaxyx stdscr
        mvaddstr (y - 1) 0 ":"
        refresh

        input <- readline
        erase
        mvaddstr 10 10 $ "input: -" ++ (show input) ++ "-"
        refresh
        runCommand input
      else
        run

main = do
  win <- initscr
  keypad stdscr True
  curs_set 0
  noecho

  finally run endwin
  return ()


-- | Read a line of input
readline :: IO (Maybe String)
readline = do
  echo
  cursorBackup <- curs_set 1
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
