{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module TextWidget (
  makeTextWidget
, TextLine (..)
, Chunk (..)
) where

import           Data.Foldable (forM_)
import           Data.String
import           Data.Default

import           Vimus
import           Ruler
import           Util (clamp)
import           Render
import           Type
import           WindowLayout

makeTextWidget :: [TextLine] -> AnyWidget
makeTextWidget content = AnyWidget def {textWidgetContent = content}

-- | A chunk of text, possibly colored.
data Chunk = Colored WindowColor String | Plain String

-- | A line of text.
newtype TextLine = TextLine [Chunk]

instance IsString TextLine where
  fromString = TextLine . return . Plain

data TextWidget = TextWidget {
  textWidgetContent  :: [TextLine]
, textWidgetViewSize :: WindowSize
, textWidgetPosition :: Int
}

instance Default TextWidget where
  def = TextWidget def def def

addChunks :: Int -> Int -> [Chunk] -> Render ()
addChunks = go
  where
    go y x chunks = case chunks of
      []   -> return ()
      c:cs -> case c of
        Plain s         -> addstr y x s                   >> go y (x + length s) cs
        Colored color s -> withColor color (addstr y x s) >> go y (x + length s) cs

instance Widget TextWidget where
  render (TextWidget content (WindowSize sizeY _) pos) = do
    forM_ (zip [0 .. pred sizeY] (drop pos content)) $ \(y, TextLine c) -> do
      addChunks y 0 c
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
