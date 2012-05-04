{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Ruler where

import           Text.Printf (printf)

import           WindowLayout
import           Render
import           Type

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

-- | Render ruler.
drawRuler :: Int -> Ruler -> Render ()
drawRuler rulerPos (Ruler text positionIndicator visibleIndicator) = do
  WindowSize _ sizeX <- getWindowSize
  let addstr_end str = addstr rulerPos x str
        where x = max 0 (sizeX - length str)
  addstr rulerPos 0 text
  addstr_end $ maybe "" (uncurry $ printf "%6d/%-6d        ") positionIndicator ++ show visibleIndicator
  chgat rulerPos [] RulerColor
