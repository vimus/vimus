module ListWidget (
  ListWidget
, newListWidget
, moveUp
, moveDown
, scrollUp
, scrollDown
, select
, renderListWidget
) where

import UI.Curses hiding (wgetch, ungetch, mvaddstr)

data ListWidget a = ListWidget {
  position :: Int
, offset    :: Int
, viewSize  :: Int
, choices   :: [a]
, renderOne :: a -> String
}

newListWidget :: (a -> String) -> [a] -> Int -> ListWidget a
newListWidget aToString aList viewSize' = ListWidget {position = 0, offset = 0, choices = aList, renderOne = aToString
, viewSize = viewSize'
}

moveUp :: ListWidget a -> ListWidget a
moveUp l = l {position = newPosition}
  where
    newPosition = max 0 (position l - 1)

moveDown :: ListWidget a -> ListWidget a
moveDown l = l {position = newPosition}
  where
    newPosition = min (length (choices l) - 1) (position l + 1)

scrollUp :: ListWidget a -> ListWidget a
scrollUp l = l {offset = newOffset, position = min currentPosition maxPosition}
  where
    currentPosition = position l
    maxPosition     = viewSize l - 1 + newOffset
    newOffset       = max 0 $ offset l - 1

scrollDown :: ListWidget a -> ListWidget a
scrollDown l = l {offset = newOffset, position = max currentPosition newOffset}
  where
    listLength      = length $ choices l
    currentPosition = position l
    newOffset       = min (listLength - 1) $ offset l + 1

select :: ListWidget a -> a
select l = choices l !! position l

renderListWidget :: Window -> ListWidget a -> IO (ListWidget a)
renderListWidget win l = do

  (sizeY, sizeX) <- getmaxyx win

  let currentPosition = position l
  let currentOffset = offset l
  let offset_  = max currentOffset (currentPosition - (sizeY - 1))
  let offset__ = min offset_ currentPosition

  let list = take sizeY $ drop offset__ $ choices l

  werase win

  let aString = ("  " ++) . renderOne l
  let putLine (y, e) = mvwaddnstr win y 0 (aString e) sizeX
  mapM_ putLine $ zip [0..] list

  let relativePosition = currentPosition - offset__
  mvwaddstr win relativePosition 0 $ "*"

  wrefresh win

  return l {offset = offset__}
