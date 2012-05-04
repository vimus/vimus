{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Input (
  InputT
, runInputT
, unGetString
, getChar
, getInputLine
, getInputLine_
, HistoryNamespace (..)
, CompletionFunction
, noCompletion

-- exported for testing
, readline
, getUnGetBuffer
) where

import           Prelude hiding (getChar)
import           Control.Applicative
import           Control.Monad.State.Strict
import qualified Data.Char as Char
import           Control.DeepSeq
import           Data.Maybe (listToMaybe)
import           Data.Map   (Map)
import qualified Data.Map as Map
import           Data.List (intercalate)

import           UI.Curses (Window)
import qualified UI.Curses as Curses
import           UI.Curses.Key

import           WindowLayout
import           Key

import           Data.List.Zipper as ListZipper
import           Data.List.Pointed hiding (modify)
import qualified Data.List.Pointed as PointedList

-- | There two history stacks, one for commands and one for search.
data HistoryNamespace = SearchHistory | CommandHistory
  deriving (Eq, Ord)

data InputState m = InputState {
  get_wch         :: m Char
, unGetBuffer     :: !String
, history         :: !(Map HistoryNamespace [String])

-- history is disabled if last input was taken from the unGetBuffer
, historyDisabled :: !Bool
}

newtype InputT m a = InputT (StateT (InputState m) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans InputT where
  lift = InputT . lift

runInputT :: Monad m => m Char -> InputT m a -> m a
runInputT get_wch_ (InputT action) = evalStateT action (InputState get_wch_ "" Map.empty True)

getChar :: Monad m => InputT m Char
getChar = InputT $ do
  st <- get
  case unGetBuffer st of
    []   -> put st {historyDisabled = False}  >> lift (get_wch st)
    x:xs -> put st {unGetBuffer = xs}         >> return x

unGetString :: Monad m => String -> InputT m ()
unGetString s
  | null s    = return ()
  | otherwise = InputT . modify $ \st -> st {historyDisabled = True, unGetBuffer = s ++ unGetBuffer st}

-- | This is only here so that test cases can inspect the unGetBuffer.
getUnGetBuffer :: Monad m => InputT m String
getUnGetBuffer = InputT (gets unGetBuffer)

-- | Add a line to the history stack.
addHistory :: Monad m => HistoryNamespace -> String -> InputT m ()
addHistory hstName x = InputT (modify f)
  where
    f st
      -- empty line, ignore
      | null x    = st

      -- history is disabled, ignore
      | disabled  = st

      -- duplicate, ignore
      | duplicate = st

      | otherwise = hst_ `deepseq` st {history = Map.insert hstName hst_ hstMap}
      where
        hstMap = history st
        hst    = Map.findWithDefault [] hstName hstMap

        -- only keep 50 lines of history
        hst_ = take 50 (x:hst)

        disabled  = historyDisabled st
        duplicate = maybe False (== x) (listToMaybe hst)

-- | Get the history stack for a given namespace.
getHistory :: Monad m => HistoryNamespace -> InputT m [String]
getHistory name = (maybe [] id . Map.lookup name) `liftM` InputT (gets history)

type CompletionFunction = String -> Either [String] String
type Suggestions = [String]
type InputBuffer = PointedList (ListZipper Char)
data EditResult = Accept String | Continue Suggestions InputBuffer | Cancel

noCompletion :: CompletionFunction
noCompletion = const (Left [])

edit :: CompletionFunction -> InputBuffer -> Char -> EditResult
edit complete buffer c
  | accept            = (Accept . toList . focus) buffer
  | cancel            = Cancel

  -- movement
  | left              = continue ListZipper.goLeft
  | right             = continue ListZipper.goRight
  | isFirst           = continue goFirst
  | isLast            = continue goLast

  -- editing
  | delete            = continue dropRight
  | c == keyBackspace = backspace

  -- history
  | previous          = historyPrevious
  | next              = historyNext

  -- completion
  | c == keyTab       = autoComplete

  -- others
  | Char.isControl c  = continue id
  | otherwise         = continue (insertLeft c)
  where
    accept    = c == '\n'  || c == keyEnter
    cancel    = c == ctrlC || c == ctrlG || c == keyEsc

    left      = c == ctrlB || c == keyLeft
    right     = c == ctrlF || c == keyRight
    isFirst   = c == ctrlA || c == keyHome
    isLast    = c == ctrlE || c == keyEnd

    delete    = c == ctrlD || c == keyDc

    previous  = c == ctrlP || c == keyUp
    next      = c == ctrlN || c == keyDown

    backspace
      | isEmpty s = Cancel
      | otherwise = continue dropLeft
      where
        s = focus buffer

    historyPrevious
      | atEnd buffer  = Continue [] buffer
      | otherwise     = (Continue [] . PointedList.modify goLast . PointedList.goRight) buffer

    historyNext
      | atStart buffer = Continue [] buffer
      | otherwise      = (Continue [] . PointedList.modify goLast . PointedList.goLeft) buffer

    autoComplete = case complete (reverse prev) of
      Right xs         -> continue (const $ ListZipper (reverse xs) next_)
      Left suggestions -> Continue suggestions buffer
      where
        ListZipper prev next_ = focus buffer

    continue = Continue [] . (`PointedList.modify` buffer)


-- | A tuple of current input, cursor position and suggestions.
type IntermediateResult = (Int, String, Suggestions)

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
--
-- Return empty string on cancel.
readline :: Monad m => CompletionFunction -> HistoryNamespace -> (IntermediateResult -> InputT m ()) -> InputT m String
readline complete hstName onUpdate = getHistory hstName >>= go [] . PointedList [] ListZipper.empty . map fromList
  where
    go suggestions buffer = do
      let ListZipper prev next = focus buffer
      onUpdate (length prev, reverse prev ++ next, suggestions)
      c <- getChar
      case edit complete buffer c of
        Accept s     -> addHistory hstName s >> return s
        Cancel       -> return ""
        Continue s buf -> go s buf

-- | Read a line of user input.
getInputLine_ :: MonadIO m => Window -> String -> HistoryNamespace -> CompletionFunction -> InputT m String
getInputLine_ = getInputLine (const $ return ())

-- | Read a line of user input.
--
-- Apply given action on each keystroke to intermediate result.
getInputLine :: MonadIO m => (String -> m ()) -> Window -> String -> HistoryNamespace -> CompletionFunction -> InputT m String
getInputLine action window prompt hstName complete = do
  r <- readline complete hstName update
  liftIO (Curses.werase window)
  return r
  where
    update r@(_, input, _) = InputT . lift $ do
      action input
      liftIO (updateWindow window prompt r)

-- | Draw intermediate result to screen.
updateWindow :: Window -> String -> IntermediateResult -> IO ()
updateWindow window prompt (cursor, input, suggestions) = do
  Curses.werase window

  let s = intercalate "   " suggestions

  -- It is important to draw everything with one single call to mvwaddstr!
  --
  -- Otherwise subsequent calls to mvwaddstr overwrite the the last column, if
  -- the start position is outside the window.
  Curses.mvwaddstr window 0 0 (prompt ++ input ++ replicate 6 ' ' ++ s)

  mvwchgat window 0 (length prompt + cursor) 1 [Reverse] InputColor

  -- color suggestions
  unless (null s) $ do
    let start = length prompt + length input + 6
    mvwchgat window 0 start (length s) [] SuggestionsColor

  return ()
