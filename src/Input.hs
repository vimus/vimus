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

import           UI.Curses hiding (wgetch, ungetch, wchgat)
import qualified UI.Curses as Curses
import           WindowLayout

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

updateWindow :: Window -> String -> String -> IO ()
updateWindow window prompt str = do
  mvwaddstr window 0 0 prompt
  waddstr window (reverse str)
  wchgat window 1 [Reverse] InputColor
  return ()

data EditState a = Accept a | Continue a | Cancel

edit :: String -> Char -> EditState String
edit str c
  | accept c          = Accept (reverse str)
  | cancel c          = Cancel
  | c == keyBackspace = backspace str
  | otherwise         = Continue (c : str)
  where
    accept = (`elem` ['\n', keyEnter])
    cancel = (`elem` ['\ETX', '\ESC'])

    backspace []     = Cancel
    backspace (_:xs) = Continue xs

-- | Read a line of user input.
readline_ :: (MonadIO m) => Window -> Char -> m Char -> m (Maybe String)
readline_ = readline (const $ return ())

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
readline :: (MonadIO m) => (String -> m ()) -> Window -> Char -> m Char -> m (Maybe String)
readline action window prompt getChar = liftIO (werase window) >> go ""
  where
    go str = do
      action str
      liftIO $ updateWindow window [prompt] str

      c <- getChar
      liftIO (werase window)
      case str `edit` c of
        Accept s   -> return (Just s)
        Cancel     -> return Nothing
        Continue s -> go s
