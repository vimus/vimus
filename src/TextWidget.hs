{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module TextWidget (
  makeTextWidget
, TextLine (..)
, Chunk (..)
) where

import           Data.Foldable (forM_)
import           Data.String

import           Vimus
import           Ruler
import           Util (clamp)
import           Render
import           Type
import           WindowLayout

makeTextWidget :: [TextLine] -> AnyWidget
makeTextWidget = AnyWidget . (`TextWidget` 0)

-- | A chunk of text, possibly colored.
data Chunk = Colored WindowColor String | Plain String

-- | A line of text.
newtype TextLine = TextLine [Chunk]

instance IsString TextLine where
  fromString = TextLine . return . Plain

data TextWidget = TextWidget [TextLine] Int

addChunks :: Int -> Int -> [Chunk] -> Render ()
addChunks = go
  where
    go y x chunks = case chunks of
      []   -> return ()
      c:cs -> case c of
        Plain s         -> addstr y x s                   >> go y (x + length s) cs
        Colored color s -> withColor color (addstr y x s) >> go y (x + length s) cs

instance Widget TextWidget where
  render (TextWidget content pos) = do
    WindowSize sizeY _ <- getWindowSize
    forM_ (zip [0 .. pred sizeY] (drop pos content)) $ \(y, TextLine c) -> do
      addChunks y 0 c
    let visibleIndicator = visible (length content) sizeY pos
    return (Ruler "" Nothing visibleIndicator)

  currentItem _    = Nothing
  searchItem w _ _ = w
  filterItem w _   = w
  handleEvent widget@(TextWidget content pos) ev = return $ case ev of
    EvMoveUp          -> scroll (-1)
    EvMoveDown        -> scroll 1
    EvMoveFirst       -> TextWidget content 0
    EvMoveLast        -> TextWidget content (pred $ length content) -- FIXME: this should be something like (length - sizeY) instead!
    EvScrollUp        -> scroll (-1)
    EvScrollDown      -> scroll 1
    EvScrollPageUp    -> widget
    EvScrollPageDown  -> widget
    _                 -> widget
    where
      scroll n = TextWidget content $ clamp 0 (length content) (pos + n)
