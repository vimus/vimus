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
import qualified Data.Char as Char
import           Control.Monad.Trans (MonadIO, liftIO)

import           UI.Curses hiding (wgetch, ungetch, wchgat, mvwchgat)
import qualified UI.Curses as Curses
import           WindowLayout
import           Key

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

-- | just a zipper
data InputBuffer = InputBuffer !String !String

dropRight :: InputBuffer -> InputBuffer
dropRight (InputBuffer xs (_:ys)) = InputBuffer xs ys
dropRight s = s

goLeft :: InputBuffer -> InputBuffer
goLeft (InputBuffer (x:xs) ys) = InputBuffer xs (x:ys)
goLeft s = s

goRight :: InputBuffer -> InputBuffer
goRight (InputBuffer xs (y:ys)) = InputBuffer (y:xs) ys
goRight s = s

goFirst :: InputBuffer -> InputBuffer
goFirst (InputBuffer xs ys) = InputBuffer [] (reverse xs ++ ys)

goLast :: InputBuffer -> InputBuffer
goLast (InputBuffer xs ys) = InputBuffer (reverse ys ++ xs) []

toString :: InputBuffer -> String
toString (InputBuffer prev next) = reverse prev ++ next

updateWindow :: Window -> String -> InputBuffer -> IO ()
updateWindow window prompt (InputBuffer prev next) = do
  mvwaddstr window 0 0 prompt
  waddstr window (reverse prev)
  waddstr window next
  mvwchgat window 0 (length prompt + length prev) 1 [Reverse] InputColor
  return ()

data EditState = Accept String | Continue InputBuffer | Cancel

edit :: InputBuffer -> Char -> EditState
edit s@(InputBuffer prev next) c
  | accept            = Accept (toString s)
  | cancel            = Cancel
  | delete            = Continue (dropRight s)
  | left              = Continue (goLeft s)
  | right             = Continue (goRight s)
  | c == keyBackspace = backspace
  | isFirst           = Continue (goFirst s)
  | isLast            = Continue (goLast s)
  | Char.isControl c  = Continue s
  | otherwise         = Continue (InputBuffer (c:prev) next)
  where
    accept    = c == '\n'  || c == keyEnter
    cancel    = c == ctrlC || c == ctrlG || c == keyEsc
    delete    = c == ctrlD || c == keyDc
    left      = c == ctrlB || c == keyLeft
    right     = c == ctrlF || c == keyRight

    isFirst   = c == ctrlA || c == keyHome
    isLast    = c == ctrlE || c == keyEnd

    backspace = case s of
      InputBuffer "" ""     -> Cancel
      InputBuffer "" _      -> Continue s
      InputBuffer (_:xs) ys -> Continue (InputBuffer xs ys)

-- | Read a line of user input.
readline_ :: (MonadIO m) => Window -> String -> m Char -> m (Maybe String)
readline_ = readline (const $ return ())

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
readline :: (MonadIO m) => (String -> m ()) -> Window -> String -> m Char -> m (Maybe String)
readline action window prompt getChar = liftIO (werase window) >> go (InputBuffer "" "")
  where
    go str = do
      action (toString str)
      liftIO $ updateWindow window prompt str

      c <- getChar
      liftIO (werase window)
      case str `edit` c of
        Accept s   -> return (Just s)
        Cancel     -> return Nothing
        Continue s -> go s
