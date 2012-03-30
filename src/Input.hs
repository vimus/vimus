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


data InputState m = InputState {
  get_wch      :: m Char
, unGetBuffer  :: String
, previousLine :: Maybe String
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
historyAdd str = InputT . modify $ \st -> st {previousLine = Just str}

-- | Move one line back in the history.
historyPrevious :: Monad m => InputT m (Maybe String)
historyPrevious = InputT (gets previousLine)

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

fromString :: String -> InputBuffer
fromString s = InputBuffer (reverse s) ""

data EditState = Accept String | Continue InputBuffer | Cancel

edit :: Monad m => InputBuffer -> Char -> InputT m EditState
edit s@(InputBuffer prev next) c
  | isAccept          = accept
  | cancel            = return Cancel
  | delete            = continue (dropRight s)
  | left              = continue (goLeft s)
  | right             = continue (goRight s)
  | c == keyBackspace = backspace
  | isFirst           = continue (goFirst s)
  | isLast            = continue (goLast s)
  | previous          = historyPrevious >>= maybe (continue s) (continue . fromString)
  | Char.isControl c  = continue s
  | otherwise         = continue (InputBuffer (c:prev) next)
  where
    isAccept  = c == '\n'  || c == keyEnter
    cancel    = c == ctrlC || c == ctrlG || c == keyEsc
    delete    = c == ctrlD || c == keyDc
    left      = c == ctrlB || c == keyLeft
    right     = c == ctrlF || c == keyRight
    previous  = c == ctrlP

    isFirst   = c == ctrlA || c == keyHome
    isLast    = c == ctrlE || c == keyEnd

    backspace = case s of
      InputBuffer "" ""     -> return Cancel
      InputBuffer "" _      -> continue s
      InputBuffer (_:xs) ys -> continue (InputBuffer xs ys)

    accept = do
      let r = toString s
      historyAdd r
      return (Accept r)

    continue = return . Continue

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
readline :: Monad m => (InputBuffer -> InputT m ()) -> InputT m (Maybe String)
readline onUpdate = go (InputBuffer "" "")
  where
    go buffer = do
      onUpdate buffer
      r <- getChar >>= edit buffer
      case r of
        Accept s   -> return (Just s)
        Cancel     -> return Nothing
        Continue buf -> go buf

-- | Read a line of user input.
getInputLine_ :: MonadIO m => Window -> String -> InputT m (Maybe String)
getInputLine_ = getInputLine (const $ return ())

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
getInputLine :: MonadIO m => (String -> m ()) -> Window -> String -> InputT m (Maybe String)
getInputLine action window prompt = do
  r <- readline update
  liftIO (Curses.werase window)
  return r
  where
    update buffer = InputT . lift $ do
      action (toString buffer)
      liftIO (updateWindow window prompt buffer)

updateWindow :: Window -> String -> InputBuffer -> IO ()
updateWindow window prompt (InputBuffer prev next) = do
  Curses.werase window
  Curses.mvwaddstr window 0 0 prompt
  Curses.waddstr window (reverse prev)
  Curses.waddstr window next
  mvwchgat window 0 (length prompt + length prev) 1 [Reverse] InputColor
  return ()
