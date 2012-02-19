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
data InputState = InputState !String !String

goLeft :: InputState -> InputState
goLeft (InputState (x:xs) ys) = InputState xs (x:ys)
goLeft s = s

goRight :: InputState -> InputState
goRight (InputState xs (y:ys)) = InputState (y:xs) ys
goRight s = s

goFirst :: InputState -> InputState
goFirst (InputState xs ys) = InputState [] (reverse xs ++ ys)

goLast :: InputState -> InputState
goLast (InputState xs ys) = InputState (reverse ys ++ xs) []

toString :: InputState -> String
toString (InputState prev next) = reverse prev ++ next

updateWindow :: Window -> String -> InputState -> IO ()
updateWindow window prompt (InputState prev next) = do
  mvwaddstr window 0 0 prompt
  waddstr window (reverse prev)
  waddstr window next
  mvwchgat window 0 (length prompt + length prev) 1 [Reverse] InputColor
  return ()

data EditState = Accept String | Continue InputState | Cancel

edit :: InputState -> Char -> EditState
edit s@(InputState prev next) c
  | accept c          = Accept (toString s)
  | cancel c          = Cancel
  | c == keyLeft      = Continue (goLeft s)
  | c == keyRight     = Continue (goRight s)
  | c == keyBackspace = backspace
  | c == keyHome      = Continue (goFirst s)
  | c == keyEnd       = Continue (goLast s)
  | Char.isControl c  = Continue s
  | otherwise         = Continue (InputState (c:prev) next)
  where
    accept = (`elem` ['\n', keyEnter])
    cancel = (`elem` ['\ETX', '\ESC'])

    backspace = case s of
      InputState "" ""     -> Cancel
      InputState "" _      -> Continue s
      InputState (_:xs) ys -> Continue (InputState xs ys)

-- | Read a line of user input.
readline_ :: (MonadIO m) => Window -> Char -> m Char -> m (Maybe String)
readline_ = readline (const $ return ())

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
readline :: (MonadIO m) => (String -> m ()) -> Window -> Char -> m Char -> m (Maybe String)
readline action window prompt getChar = liftIO (werase window) >> go (InputState "" "")
  where
    go str = do
      action (toString str)
      liftIO $ updateWindow window [prompt] str

      c <- getChar
      liftIO (werase window)
      case str `edit` c of
        Accept s   -> return (Just s)
        Cancel     -> return Nothing
        Continue s -> go s
