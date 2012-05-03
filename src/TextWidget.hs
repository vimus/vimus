{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module TextWidget (makeTextWidget) where

import           Data.Foldable (forM_)

import           UI.Curses hiding (wgetch, ungetch, mvaddstr, err)

import           Vimus
import           Util (clamp)

makeTextWidget :: [String] -> Int -> Widget
makeTextWidget c p = Widget (TextWidget c p)

data TextWidget = TextWidget [String] Int
  deriving (Eq, Show)

instance IsWidget TextWidget where
  render (TextWidget content pos) window = do
    (sizeY, sizeX) <- getmaxyx window
    forM_ (zip [0 .. pred sizeY] (drop pos content)) $ \(y, c) -> do
      mvwaddnstr window y 0 c sizeX
    return ()

  event (TextWidget content pos) ev = return $ case ev of
    EvMoveUp          -> scroll (-1)
    EvMoveDown        -> scroll 1
    EvMoveFirst       -> Just $ TextWidget content 0
    EvMoveLast        -> Just $ TextWidget content (pred $ length content) -- FIXME
    EvScrollUp        -> scroll (-1)
    EvScrollDown      -> scroll 1
    EvScrollPageUp    -> Nothing
    EvScrollPageDown  -> Nothing
    _                 -> Nothing
    where
      scroll n = Just (TextWidget content $ clamp 0 (length content) (pos + n))

  currentItem _    = Nothing
  searchItem w _ _ = Just w
  filterItem w _   = Just w

