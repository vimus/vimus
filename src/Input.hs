module Input (
  wgetch
, readline
, readline_
) where

import Prelude hiding (getChar)

import UI.Curses hiding (wgetch, ungetch)
import qualified UI.Curses as Curses

import Control.Monad.Trans (MonadIO, liftIO)


wgetch :: (MonadIO m) => Window -> m Char
wgetch = liftIO . Curses.wget_wch

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
      liftIO $ mvwaddstr win 0 1 str
      liftIO $ wclrtoeol win
      liftIO $ wchgat win 1 [Reverse] 1
      liftIO $ wrefresh win

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
