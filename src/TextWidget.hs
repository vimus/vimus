{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module TextWidget (makeTextWidget) where

import           Data.Foldable (forM_)

import           Vimus
import           Ruler
import           Util (clamp)
import           Render
import           Type

makeTextWidget :: [String] -> Int -> AnyWidget
makeTextWidget c p = AnyWidget (TextWidget c p)

data TextWidget = TextWidget [String] Int
  deriving (Eq, Show)

instance Widget TextWidget where
  render (TextWidget content pos) = do
    WindowSize sizeY _ <- getWindowSize
    let rulerPos = pred sizeY
        viewSize = rulerPos
        visibleIndicator = visible (length content) viewSize pos
    forM_ (zip [0 .. pred viewSize] (drop pos content)) $ \(y, c) -> do
      addstr y 0 c

    drawRuler rulerPos (Ruler "" Nothing visibleIndicator)

  currentItem _    = Nothing
  searchItem w _ _ = w
  filterItem w _   = w
  handleEvent widget@(TextWidget content pos) ev = return $ case ev of
    EvMoveUp          -> scroll (-1)
    EvMoveDown        -> scroll 1
    EvMoveFirst       -> TextWidget content 0
    EvMoveLast        -> TextWidget content (pred $ length content) -- FIXME
    EvScrollUp        -> scroll (-1)
    EvScrollDown      -> scroll 1
    EvScrollPageUp    -> widget
    EvScrollPageDown  -> widget
    _                 -> widget
    where
      scroll n = TextWidget content $ clamp 0 (length content) (pos + n)
