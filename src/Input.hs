module Input (
  wgetch
, ungetstr
, readline
) where

import UI.Curses hiding (wgetch, ungetch)
import qualified UI.Curses as Curses

import Foreign (unsafePerformIO)
import Data.IORef
import Control.Monad.Trans (MonadIO, liftIO)

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

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
readline :: (MonadIO m) => (String -> m ()) -> Window -> m (Maybe String)
readline action win = _readline ""
  where
    _readline str = do
      action str
      liftIO $ mvwaddstr win 0 1 str
      liftIO $ wclrtoeol win
      liftIO $ wchgat win 1 [Reverse] 1
      liftIO $ wrefresh win

      c <- liftIO $ wgetch win

      let continue | accept c          = return $ Just str
                   | cancel c          = return Nothing
                   | c == keyBackspace = backspace str
                   | otherwise         = _readline $ str ++ [c]
      continue

      where
        accept c = c == '\n' || c == keyF 1 || c == keyEnter
        cancel c = c `elem` ['\3', '\27']

        backspace [] = return Nothing
        backspace s = _readline $ init s
