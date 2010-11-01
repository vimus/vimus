module Input (
  wgetch
, ungetstr
, readline
) where

import UI.Curses hiding (wgetch, ungetch)
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


wgetch :: Window -> IO Char
wgetch win = do
  queue <- readIORef inputQueue
  getChar_ queue
  where
    getChar_ []     = Curses.wget_wch win
    getChar_ (x:xs) = do
      writeIORef inputQueue xs
      return x

------------------------------------------------------------------------
-- | Read a line of input
readline :: Window -> IO (Maybe String)
readline win = do
  echo
  -- cursorBackup <- curs_set 1
  wclrtoeol win
  input <- readline_ ""
  -- _ <- curs_set cursorBackup
  noecho
  return $ fmap reverse input
  where
    readline_ str = do
      c <- wgetch win

      let continue | accept c          = return $ Just str
                   | c == keyBackspace = backspace str
                   | otherwise         = readline_ $ c : str
      continue

      where
        accept c = c == '\n' || c == keyF 1 || c == keyEnter

        backspace [] = return Nothing
        backspace (_:s)= do
          wdelch win
          readline_ s
