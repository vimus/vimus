module Input (
  wgetch
, ungetstr
, readline
, readline_
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
ungetstr :: MonadIO m => String -> m ()
ungetstr s = liftIO $ do
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
readline_ :: Window -> Char -> IO (Maybe String)
readline_ = readline (const $ return ())

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
readline :: (MonadIO m) => (String -> m ()) -> Window -> Char -> m (Maybe String)
readline action win prompt = do
  liftIO $ mvwaddstr win 0 0 [prompt]
  r <- _readline ""
  liftIO $ werase win
  liftIO $ wrefresh win
  return r

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
        accept = (`elem` ['\n', keyEnter])
        cancel = (`elem` ['\ETX', '\ESC'])

        backspace [] = return Nothing
        backspace s = _readline $ init s
