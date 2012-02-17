{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Input (
  wgetch
, ungetstr
, readline
, readline_
) where

import           Prelude hiding (getChar)

import           System.IO.Unsafe (unsafePerformIO)
import           Data.IORef
import           Control.Monad.Trans (MonadIO, liftIO)

import           UI.Curses hiding (wgetch, ungetch)
import qualified UI.Curses as Curses

-- Ncurses uses a bounded FIFO for ungetch, so we can not use it to put
-- arbitrary-length strings back into the queue.  For now we use the
-- unsafePerformIO hack to work around this.
inputQueue :: IORef String
{-# NOINLINE inputQueue #-}
inputQueue = unsafePerformIO (newIORef "")

-- | Push given string into input queue.
ungetstr :: (MonadIO m) => String -> m ()
ungetstr s = liftIO $ do
  old <- readIORef inputQueue
  writeIORef inputQueue $ s ++ old


wgetch :: (MonadIO m) => Window -> m Char
wgetch win = liftIO $ do
  queue <- readIORef inputQueue
  getChar_ queue
  where
    getChar_ []     = Curses.wget_wch win
    getChar_ (x:xs) = do
      writeIORef inputQueue xs
      return x

------------------------------------------------------------------------

-- | Read a line of user input.
readline_ :: (MonadIO m) => Window -> Char -> m Char -> m (Maybe String)
readline_ = readline (const $ return ())

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
readline :: (MonadIO m) => (String -> m ()) -> Window -> Char -> m Char -> m (Maybe String)
readline action win prompt getChar = do
  liftIO $ mvwaddstr win 0 0 [prompt]
  r <- _readline ""
  liftIO $ werase win
  liftIO $ wrefresh win
  return r

  where
    _readline str = do
      action str
      liftIO $ do
        mvwaddstr win 0 1 str
        wclrtoeol win
        wchgat win 1 [Reverse] 1
        wrefresh win

      c <- getChar

      let continue | accept c          = return $ Just str
                   | cancel c          = return Nothing
                   | c == keyBackspace = backspace str
                   | otherwise         = _readline $ str ++ [c]
      continue

      where
        accept = (`elem` ['\n', keyEnter])
        cancel = (`elem` ['\ETX', '\ESC'])

        backspace [] = return Nothing
        backspace s = _readline $ init s
