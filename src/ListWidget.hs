{-# LANGUAGE CPP #-}
module ListWidget
#ifndef TEST
( ListWidget
, new
, update
, search
, searchBackward
, moveUp
, moveDown
, moveLast
, moveFirst
, scrollUp
, scrollDown
, scrollPageUp
, scrollPageDown
, setViewSize
, select
, render
)
#endif
where

import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)

import UI.Curses hiding (wgetch, ungetch, mvaddstr)

data ListWidget a = ListWidget {
  getPosition   :: Int
, getOffset     :: Int
, getList       :: [a]
, getListLength :: Int
, getViewSize   :: Int          -- ^ number of lines that can be displayed at once
} deriving Show



new :: [a] -> Int -> ListWidget a
new aList viewSize = ListWidget
                    { getPosition = 0
                    , getOffset   = 0
                    , getList     = aList
                    , getListLength = length aList
                    , getViewSize = viewSize
                    }

setViewSize :: ListWidget a -> Int -> ListWidget a
setViewSize widget viewSize = result
  where
    w = widget { getViewSize = viewSize }
    -- to make sure that offset is correct, we simply set position
    result = setPosition w $ getPosition w


update :: ListWidget a -> [a] -> ListWidget a
update widget list = widget {
                      getPosition   = newPosition
                    , getList       = list
                    , getListLength = newListLength
                    }
  where
    newListLength   = length list
    currentPosition = getPosition widget
    newPosition     = min currentPosition (max 0 $ newListLength - 1)

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
        n = getPosition widget + 1
        enumeratedList = zip [0..] $ getList widget

searchBackward :: (a -> Bool) -> ListWidget a -> ListWidget a
searchBackward predicate widget = maybe widget (setPosition widget) match
  where
    match = findFirst predicate shiftedList
    -- rotate list, to get next match from current position
    shiftedList = reverse $ rotate n enumeratedList
      where
        n = getPosition widget
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

-- |
-- Confine a number to an interval.  The result will be greater or equal to a
-- given lower bound and (if still possible) smaller than a given upper bound.
confine :: Int -- ^ lower bound (inclusive)
        -> Int -- ^ upper bound (exclusive)
        -> Int
        -> Int
confine lower upper n = max lower $ min (upper -1) n

setPosition :: ListWidget a -> Int -> ListWidget a
setPosition widget pos = widget { getPosition = newPosition, getOffset = newOffset }
  where
    newPosition   = confine 0 listLength pos
    listLength    = getListLength widget
    currentOffset = getOffset widget
    minOffset     = newPosition - (getViewSize widget - 1)
    newOffset     = max minOffset $ min currentOffset newPosition

moveFirst :: ListWidget a -> ListWidget a
moveFirst l = setPosition l 0

moveLast :: ListWidget a -> ListWidget a
moveLast l = setPosition l $ getListLength l - 1

-- TODO: define moveUp and moveDown in terms of setPosition
moveUp :: ListWidget a -> ListWidget a
moveUp l = l {getPosition = newPosition, getOffset = min currentOffset newPosition}
  where
    currentOffset = getOffset l
    newPosition   = max 0 (getPosition l - 1)

moveDown :: ListWidget a -> ListWidget a
moveDown l = l {getPosition = newPosition, getOffset = max currentOffset minOffset}
  where
    currentPosition = getPosition l
    currentOffset   = getOffset l
    newPosition     = min (max 0 $ getListLength l - 1) (currentPosition + 1)
    minOffset       = newPosition - (getViewSize l - 1)


scrollUp_ :: Int -> ListWidget a -> ListWidget a
scrollUp_ n l = l {getOffset = newOffset, getPosition = min currentPosition maxPosition}
  where
    currentPosition = getPosition l
    maxPosition     = getViewSize l - 1 + newOffset
    newOffset       = max 0 $ getOffset l - n

scrollDown_ :: Int -> ListWidget a -> ListWidget a
scrollDown_ n l = l {getOffset = newOffset, getPosition = max currentPosition newOffset}
  where
    currentPosition = getPosition l
    newOffset       = min (max 0 $ getListLength l - 1) $ getOffset l + n

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
             then Just $ getList l !! getPosition l
             else Nothing

render :: MonadIO m => ListWidget a -> (a -> String) -> Window -> m ()
render l renderOne win = liftIO $ do

  werase win

  when (getListLength l > 0) $ do
    let viewSize = getViewSize l
    (_, sizeX) <- getmaxyx win

    let currentPosition = getPosition l
    let currentOffset = getOffset l
    let list = take viewSize $ drop currentOffset $ getList l

    let putLine (y, element) = mvwaddnstr win y 0 (renderOne element) sizeX
    mapM_ putLine $ zip [0..] list

    let relativePosition = currentPosition - currentOffset
    mvwchgat win relativePosition 0 (-1) [Reverse] 2
    return ()

  wrefresh win
  return ()
