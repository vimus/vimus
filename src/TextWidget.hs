{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module TextWidget (makeTextWidget) where

import           Data.Foldable (forM_)
import           Data.Default

import           UI.Curses hiding (wgetch, ungetch, mvaddstr, err)

import           Vimus
import           Util (clamp)

makeTextWidget :: [String] -> Int -> Widget
makeTextWidget content pos = def {
    render = \window -> do
      (sizeY, sizeX) <- getmaxyx window
      forM_ (zip [0 .. pred sizeY] (drop pos content)) $ \(y, c) -> do
        mvwaddnstr window y 0 c sizeX
      return ()

  , event = \ev -> return $ case ev of
      EvMoveUp          -> scroll (-1)
      EvMoveDown        -> scroll 1
      EvMoveFirst       -> Just $ makeTextWidget content 0
      EvMoveLast        -> Just $ makeTextWidget content (pred $ length content) -- FIXME
      EvScrollUp        -> scroll (-1)
      EvScrollDown      -> scroll 1
      EvScrollPageUp    -> Nothing
      EvScrollPageDown  -> Nothing
      _                 -> Nothing
  }
  where
    scroll n = Just (makeTextWidget content $ clamp 0 (length content) (pos + n))

