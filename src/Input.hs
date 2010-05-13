module Input (
  getch
, ungetstr
, readline
) where

import UI.Curses hiding (getch, ungetch)
import qualified UI.Curses as Curses

import Foreign (unsafePerformIO)
import Data.IORef

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
        backspace (_:s)= do
          delch
          readline_ s
