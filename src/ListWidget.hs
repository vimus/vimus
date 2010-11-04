module ListWidget (
  ListWidget
, newListWidget
, update
, search
, searchBackward
, moveUp
, moveDown
, scrollUp
, scrollDown
, scrollPageUp
, scrollPageDown
, select
, render
) where

import Control.Monad.Trans (MonadIO, liftIO)

import UI.Curses hiding (wgetch, ungetch, mvaddstr)

data ListWidget a = ListWidget {
  position    :: Int
, offset      :: Int
, getList     :: [a]
, getListLength :: Int
, renderOne   :: a -> String
, getView     :: Window       -- ^ the window, this widget is rendered to
, getViewSize :: Int          -- ^ number of lines that can be displayed at once
}

newListWidget :: (a -> String) -> [a] -> Window -> IO (ListWidget a)
newListWidget aToString aList window = do
  (sizeY, _) <- getmaxyx window
  return ListWidget { position    = 0
                    , offset      = 0
                    , getList     = aList
                    , getListLength = length aList
                    , renderOne   = aToString
                    , getViewSize = sizeY
                    , getView     = window
                    }

update :: ListWidget a -> [a] -> ListWidget a
update widget list = widget {
                      position      = newPosition
                    , getList       = list
                    , getListLength = newListLength
                    }
  where
    newListLength   = length list
    currentPosition = position widget
    newPosition     = min currentPosition $ (max 0 $ newListLength - 1)

------------------------------------------------------------------------
-- search


-- | Rotate elements of given list by given number.
--
-- >>> rotate 3 [0..10]
-- [3,4,5,6,7,8,9,10,0,1,2]
rotate :: Int -> [a] -> [a]
rotate n l = drop n l ++ take n l

search :: (a -> Bool) -> ListWidget a -> ListWidget a
search predicate widget = maybe widget (setPosition widget) match
  where
    match = findFirst predicate shiftedList
    -- rotate list, to get next match from current position
    shiftedList = rotate n enumeratedList
      where
        n = position widget + 1
        enumeratedList = zip [0..] $ getList widget

searchBackward :: (a -> Bool) -> ListWidget a -> ListWidget a
searchBackward predicate widget = maybe widget (setPosition widget) match
  where
    match = findFirst predicate shiftedList
    -- rotate list, to get next match from current position
    shiftedList = reverse $ rotate n enumeratedList
      where
        n = position widget
        enumeratedList = zip [0..] $ getList widget

findFirst :: (a -> Bool) -> [(Int, a)] -> Maybe Int
findFirst predicate list = case matches of
  (n, _):_  -> Just n
  _         -> Nothing
  where
    matches = filter predicate_ list
      where
        predicate_ (_, y) = predicate y

------------------------------------------------------------------------
-- move

setPosition :: ListWidget a -> Int -> ListWidget a
setPosition widget newPosition = widget { position = newPosition, offset = newOffset }
  where
    currentOffset = offset widget
    minOffset     = newPosition - (getViewSize widget - 1)
    newOffset     = max minOffset $ min currentOffset newPosition

-- TODO: define moveUp and moveDown in terms of setPosition
moveUp :: ListWidget a -> ListWidget a
moveUp l = l {position = newPosition, offset = min currentOffset newPosition}
  where
    currentOffset = offset l
    newPosition   = max 0 (position l - 1)

moveDown :: ListWidget a -> ListWidget a
moveDown l = l {position = newPosition, offset = max currentOffset minOffset}
  where
    currentPosition = position l
    currentOffset   = offset l
    newPosition     = min (max 0 $ getListLength l - 1) (currentPosition + 1)
    minOffset       = newPosition - (getViewSize l - 1)


scrollUp_ :: Int -> ListWidget a -> ListWidget a
scrollUp_ n l = l {offset = newOffset, position = min currentPosition maxPosition}
  where
    currentPosition = position l
    maxPosition     = getViewSize l - 1 + newOffset
    newOffset       = max 0 $ offset l - n

scrollDown_ :: Int -> ListWidget a -> ListWidget a
scrollDown_ n l = l {offset = newOffset, position = max currentPosition newOffset}
  where
    currentPosition = position l
    newOffset       = min (max 0 $ getListLength l - 1) $ offset l + n

-- | offset for page scroll
pageScroll :: ListWidget a -> Int
pageScroll l = max 0 $ getViewSize l - 2

scrollUp, scrollPageUp :: ListWidget a -> ListWidget a
scrollUp       = scrollUp_ 1
scrollPageUp l = scrollUp_ (pageScroll l) l

scrollDown, scrollPageDown :: ListWidget a -> ListWidget a
scrollDown       = scrollDown_ 1
scrollPageDown l = scrollDown_ (pageScroll l) l


select :: ListWidget a -> Maybe a
select l = if getListLength l > 0
             then Just $ getList l !! position l
             else Nothing

render :: MonadIO m => ListWidget a -> m ()
render l = liftIO $ do

  let win = getView l
  werase win

  if getListLength l > 0
    then do
      (sizeY, sizeX) <- getmaxyx win

      let currentPosition = position l
      let currentOffset = offset l
      let list = take sizeY $ drop currentOffset $ getList l

      let putLine (y, element) = mvwaddnstr win y 0 (renderOne l $  element) sizeX
      mapM_ putLine $ zip [0..] list

      let relativePosition = currentPosition - currentOffset
      mvwchgat win relativePosition 0 (-1) [Reverse] 2
      return ()
    else
      return ()

  wrefresh win
  return ()
