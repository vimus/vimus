{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Ruler where

import           Text.Printf (printf)
import           UI.Curses hiding (wgetch, ungetch, mvaddstr, mvwchgat)
import           WindowLayout

type PositionIndicator = Maybe (Int, Int)

data Ruler = Ruler String PositionIndicator Visible
  deriving (Eq, Show)

-- | A vim-like "visible" indicator.
data Visible = All | Top | Bot | Percent Int
  deriving Eq

instance Show Visible where
  show v = case v of
    All       -> "All"
    Top       -> "Top"
    Bot       -> "Bot"
    Percent n -> printf "%2d%%" n

-- | Calculate a vim-like "visible" indicator.
visible :: Int -> Int -> Int -> Visible
visible size viewSize pos
  | topVisible && botVisible = All
  | topVisible               = Top
  | botVisible               = Bot
  | otherwise                = Percent $ (pos * 100) `div` (size - viewSize)
  where
    topVisible = pos == 0
    botVisible = size <= pos + viewSize

drawRuler :: Window -> Int -> Ruler -> IO ()
drawRuler window rulerPos (Ruler text positionIndicator visibleIndicator) = do
  (_, sizeX) <- getmaxyx window
  -- draw ruler
  let addstr_end str = mvwaddnstr window rulerPos x str (sizeX - x)
        where x = max 0 (sizeX - length str)
  mvwaddnstr window rulerPos 0 text sizeX
  addstr_end $ maybe "" (uncurry $ printf "%6d/%-6d        ") positionIndicator ++ show visibleIndicator
  mvwchgat window rulerPos 0 (-1) [] RulerColor
  return ()
