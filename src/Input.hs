{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Input (
  InputT
, runInputT
, unGetString
, getChar
, getInputLine
, getInputLine_

-- exported for testing
, readline
) where

import           Prelude hiding (getChar)
import           Control.Applicative
import           Control.Monad.State.Strict
import qualified Data.Char as Char

import           UI.Curses (Window, Attribute(..))
import qualified UI.Curses as Curses
import           UI.Curses.Key

import           WindowLayout
import           Key

import           Data.List.Zipper as ListZipper
import           Data.List.Pointed hiding (modify)
import qualified Data.List.Pointed as PointedList


data InputState m = InputState {
  get_wch     :: m Char
, unGetBuffer :: String
, history     :: [String]
}

newtype InputT m a = InputT (StateT (InputState m) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans InputT where
  lift = InputT . lift

runInputT :: Monad m => m Char -> InputT m a -> m a
runInputT get_wch_ (InputT action) = evalStateT action (InputState get_wch_ "" [])

getChar :: Monad m => InputT m Char
getChar = InputT $ do
  st <- get
  case unGetBuffer st of
    []   -> lift (get_wch st)
    x:xs -> put st {unGetBuffer = xs} >> return x

unGetString :: Monad m => String -> InputT m ()
unGetString s = InputT . modify $ \st -> st {unGetBuffer = s ++ unGetBuffer st}

-- | Add a line to the history.
historyAdd :: Monad m => String -> InputT m ()
historyAdd str = InputT . modify $ \st -> st {history = str : history st}

type InputBuffer = PointedList (ListZipper Char)
data EditResult = Accept String | Continue InputBuffer | Cancel

edit :: Monad m => InputBuffer -> Char -> InputT m EditResult
buffer `edit` c
  | isAccept          = accept
  | cancel            = return Cancel

  -- movement
  | left              = continue ListZipper.goLeft
  | right             = continue ListZipper.goRight
  | isFirst           = continue goFirst
  | isLast            = continue goLast

  -- editing
  | delete            = continue dropRight
  | c == keyBackspace = backspace

  -- history
  | c == ctrlP        = historyPrevious
  | c == ctrlN        = historyNext

  -- others
  | Char.isControl c  = continue id
  | otherwise         = continue (insertLeft c)
  where
    isAccept  = c == '\n'  || c == keyEnter
    cancel    = c == ctrlC || c == ctrlG || c == keyEsc

    left      = c == ctrlB || c == keyLeft
    right     = c == ctrlF || c == keyRight
    isFirst   = c == ctrlA || c == keyHome
    isLast    = c == ctrlE || c == keyEnd

    delete    = c == ctrlD || c == keyDc

    backspace
      | isEmpty s = return Cancel
      | otherwise = continue dropLeft
      where
        s = focus buffer

    accept = do
      let r = toList (focus buffer)
      historyAdd r
      return (Accept r)

    historyPrevious =
      (return . Continue . PointedList.modify goLast . PointedList.goRight) buffer

    historyNext =
      (return . Continue . PointedList.modify goLast . PointedList.goLeft) buffer

    continue = return . Continue . (`PointedList.modify` buffer)

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
--
-- Return empty string on cancel.
readline :: Monad m => (ListZipper Char -> InputT m ()) -> InputT m String
readline onUpdate = InputT (gets history) >>= go . PointedList [] ListZipper.empty . map fromList
  where
    go buffer = do
      onUpdate (focus buffer)
      r <- getChar >>= edit buffer
      case r of
        Accept s   -> return s
        Cancel     -> return ""
        Continue buf -> go buf

-- | Read a line of user input.
getInputLine_ :: MonadIO m => Window -> String -> InputT m String
getInputLine_ = getInputLine (const $ return ())

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
getInputLine :: MonadIO m => (String -> m ()) -> Window -> String -> InputT m String
getInputLine action window prompt = do
  r <- readline update
  liftIO (Curses.werase window)
  return r
  where
    update buffer = InputT . lift $ do
      action (toList buffer)
      liftIO (updateWindow window prompt buffer)

updateWindow :: Window -> String -> ListZipper Char -> IO ()
updateWindow window prompt (ListZipper prev next) = do
  Curses.werase window
  Curses.mvwaddstr window 0 0 prompt
  Curses.waddstr window (reverse prev)
  Curses.waddstr window next
  mvwchgat window 0 (length prompt + length prev) 1 [Reverse] InputColor
  return ()
