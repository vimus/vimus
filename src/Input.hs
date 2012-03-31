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


data InputState m = InputState {
  get_wch     :: m Char
, unGetBuffer :: String
, history     :: Maybe String
}

newtype InputT m a = InputT (StateT (InputState m) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans InputT where
  lift = InputT . lift

runInputT :: Monad m => m Char -> InputT m a -> m a
runInputT get_wch_ (InputT action) = evalStateT action (InputState get_wch_ "" Nothing)

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
historyAdd str = InputT . modify $ \st -> st {history = Just str}

-- | Move one line back in the history.
historyPrevious :: Monad m => InputT m (Maybe String)
historyPrevious = InputT (gets history)

type InputBuffer = ListZipper Char
data EditResult = Accept String | Continue InputBuffer | Cancel

edit :: Monad m => InputBuffer -> Char -> InputT m EditResult
edit s c
  | isAccept          = accept
  | cancel            = return Cancel

  -- movement
  | left              = continue (goLeft s)
  | right             = continue (goRight s)
  | isFirst           = continue (goFirst s)
  | isLast            = continue (goLast s)

  -- editing
  | delete            = continue (dropRight s)
  | c == keyBackspace = backspace

  -- history
  | previous          = historyPrevious >>= maybe (continue s) (continue . goLast . fromList)

  -- others
  | Char.isControl c  = continue s
  | otherwise         = continue (c `insertLeft` s)
  where
    isAccept  = c == '\n'  || c == keyEnter
    cancel    = c == ctrlC || c == ctrlG || c == keyEsc

    left      = c == ctrlB || c == keyLeft
    right     = c == ctrlF || c == keyRight
    isFirst   = c == ctrlA || c == keyHome
    isLast    = c == ctrlE || c == keyEnd

    delete    = c == ctrlD || c == keyDc

    previous  = c == ctrlP


    backspace
      | isEmpty s = return Cancel
      | otherwise = continue (dropLeft s)

    accept = do
      let r = toList s
      historyAdd r
      return (Accept r)

    continue = return . Continue

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
--
-- Return empty string on cancel.
readline :: Monad m => (InputBuffer -> InputT m ()) -> InputT m String
readline onUpdate = go ListZipper.empty
  where
    go buffer = do
      onUpdate buffer
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

updateWindow :: Window -> String -> InputBuffer -> IO ()
updateWindow window prompt (ListZipper prev next) = do
  Curses.werase window
  Curses.mvwaddstr window 0 0 prompt
  Curses.waddstr window (reverse prev)
  Curses.waddstr window next
  mvwchgat window 0 (length prompt + length prev) 1 [Reverse] InputColor
  return ()
