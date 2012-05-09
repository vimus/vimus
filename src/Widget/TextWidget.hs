module Widget.TextWidget (makeTextWidget) where

import           Data.Foldable (forM_)
import           Data.Default

import           Vimus
import           Ruler
import           Util (clamp)
import           Render
import           Widget.Type

makeTextWidget :: [TextLine] -> AnyWidget
makeTextWidget content = AnyWidget def {textWidgetContent = content}

data TextWidget = TextWidget {
  textWidgetContent  :: [TextLine]
, textWidgetViewSize :: WindowSize
, textWidgetPosition :: Int
}

instance Default TextWidget where
  def = TextWidget def def def

instance Widget TextWidget where
  render (TextWidget content (WindowSize sizeY _) pos) = do
    forM_ (zip [0 .. pred sizeY] (drop pos content)) $ \(y, c) -> do
      addLine y 0 c
    let visibleIndicator = visible (length content) sizeY pos
    return (Ruler "" Nothing visibleIndicator)

  currentItem _    = Nothing
  searchItem w _ _ = w
  filterItem w _   = w
  handleEvent widget@(TextWidget content (WindowSize sizeY _) pos) ev = return $ case ev of
    EvResize size     -> widget {textWidgetViewSize = size}
    EvMoveUp          -> scroll (-1)
    EvMoveDown        -> scroll 1
    EvMoveFirst       -> widget {textWidgetPosition = 0}
    EvMoveLast        -> moveLast
    EvScroll n        -> scroll n
    _                 -> widget
    where
      scroll n = widget {textWidgetPosition = clamp 0 (length content) (pos + n)}
      moveLast
        -- if current position is greater than new position, keep it
        | newPos < pos = widget
        | otherwise    = widget {textWidgetPosition = newPos}
        where
          newPos = max (length content - sizeY) 0
