{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module ListWidget
#ifndef TEST
( ListWidget
, new
, newChild
, breadcrumbs
, hasParent
, null
, getPosition
, getParent
, getParentItem
, update
, filter
, Searchable
, searchTags
, search
, searchBackward
, setPosition
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
, selectAt
, render
)
#endif
where

import Prelude hiding (filter, null)
import qualified Prelude

import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)

import UI.Curses hiding (wgetch, ungetch, mvaddstr, mvwchgat)
import WindowLayout

data ListWidget a = ListWidget {
  getPosition     :: Int
, getList         :: [a]
, getListLength   :: Int
, getViewSize     :: Int -- ^ number of lines that can be displayed at once
, getViewPosition :: Int -- ^ position of viewport within the list
, getParent       :: Maybe (ListWidget a)
} deriving Show


null :: ListWidget a -> Bool
null = Prelude.null . getList

new :: [a] -> Int -> ListWidget a
new list viewSize = setViewSize widget viewSize
  where
    widget = ListWidget {
        getPosition = 0
      , getList = list
      , getListLength = length list
      , getViewSize = 0
      , getViewPosition = 0
      , getParent = Nothing
      }

newChild :: [a] -> ListWidget a -> ListWidget a
newChild list this = setViewSize widget $ getViewSize this
  where
    widget = ListWidget {
        getPosition = 0
      , getList = list
      , getListLength = length list
      , getViewSize = 0
      , getViewPosition = 0
      , getParent = Just this
      }

setViewSize :: ListWidget a -> Int -> ListWidget a
setViewSize widget viewSize = result
  where
    w = widget { getViewSize = max 1 viewSize }
    -- to make sure that viewPosition is correct, we simply set position
    result = setPosition w $ getPosition w


update :: ListWidget a -> [a] -> ListWidget a
update widget list = setPosition newWidget $ getPosition widget
  where
    newWidget       = widget { getList = list, getListLength = length list }


------------------------------------------------------------------------
-- parent interaction

hasParent :: ListWidget a -> Bool
hasParent list = case getParent list of
  Just _  -> True
  Nothing -> False

getParentItem :: ListWidget a -> Maybe a
getParentItem list = getParent list >>= select

------------------------------------------------------------------------
-- breadcrumbs

breadcrumbs :: Show a => ListWidget a -> String
breadcrumbs list = case getParent list of
  Just p  -> breadcrumbs p ++ " > " ++ this
  Nothing -> this
  where
    this = case select list of
      Nothing -> ""
      Just a  -> show a

------------------------------------------------------------------------
-- search

class Searchable a where
  searchTags :: a -> [String]

filter :: Searchable a => (a -> Bool) -> ListWidget a -> ListWidget a
filter predicate widget = update widget $ Prelude.filter predicate $ getList widget

-- | Rotate elements of given list by given number.
--
-- >>> rotate 3 [0..10]
-- [3,4,5,6,7,8,9,10,0,1,2]
rotate :: Int -> [a] -> [a]
rotate n l = drop n l ++ take n l

search :: Searchable a => (a -> Bool) -> ListWidget a -> ListWidget a
search predicate widget = maybe widget (setPosition widget) match
  where
    match = findFirst predicate shiftedList
    -- rotate list, to get next match from current position
    shiftedList = rotate n enumeratedList
      where
        n = getPosition widget + 1
        enumeratedList = zip [0..] $ getList widget

searchBackward :: Searchable a => (a -> Bool) -> ListWidget a -> ListWidget a
searchBackward predicate widget = maybe widget (setPosition widget) match
  where
    match = findFirst predicate shiftedList
    -- rotate list, to get next match from current position
    shiftedList = reverse $ rotate n enumeratedList
      where
        n = getPosition widget
        enumeratedList = zip [0..] $ getList widget

findFirst :: Searchable a => (a -> Bool) -> [(Int, a)] -> Maybe Int
findFirst predicate list = case matches of
  (n, _):_  -> Just n
  _         -> Nothing
  where
    matches = Prelude.filter predicate_ list
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
setPosition widget pos = widget { getPosition = newPosition, getViewPosition = newViewPosition }
  where
    newPosition     = confine 0 listLength pos
    listLength      = getListLength widget
    viewPosition    = getViewPosition widget
    minViewPosition = newPosition - (getViewSize widget - 1)
    newViewPosition = max minViewPosition $ min viewPosition newPosition

moveFirst :: ListWidget a -> ListWidget a
moveFirst l = setPosition l 0

moveLast :: ListWidget a -> ListWidget a
moveLast l = setPosition l $ getListLength l - 1

moveUp :: ListWidget a -> ListWidget a
moveUp l = setPosition l (getPosition l - 1)

moveDown :: ListWidget a -> ListWidget a
moveDown l = setPosition l (getPosition l + 1)


setViewPosition :: ListWidget a -> Int -> ListWidget a
setViewPosition l n = l {getViewPosition = newViewPosition, getPosition = newPosition}
  where
    newViewPosition = confine 0 listLength n
    newPosition     = confine newViewPosition (newViewPosition + viewSize) currentPosition
    currentPosition = getPosition l
    viewSize        = getViewSize l
    listLength      = getListLength l


scrollUp_ :: Int -> ListWidget a -> ListWidget a
scrollUp_ n l = l {getViewPosition = newViewPosition, getPosition = min currentPosition maxPosition}
  where
    currentPosition = getPosition l
    maxPosition     = getViewSize l - 1 + newViewPosition
    newViewPosition = max 0 $ getViewPosition l - n

scrollDown_ :: Int -> ListWidget a -> ListWidget a
scrollDown_ n l = l {getViewPosition = newViewPosition, getPosition = max currentPosition newViewPosition}
  where
    currentPosition = getPosition l
    newViewPosition = min (max 0 $ getListLength l - 1) $ getViewPosition l + n

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
select l =
  if getListLength l > 0
    then Just $ getList l !! getPosition l
    else Nothing


-- |
-- Return elment at given index.  Indices may be negative, to start counting
-- from the back of the list.
selectAt :: ListWidget a -> Int -> a
selectAt l n = getList l !! (n `mod` getListLength l)


render :: MonadIO m => ListWidget a -> (a -> String) -> Window -> m ()
render l renderOne window = liftIO $ do

  werase window

  when (getListLength l > 0) $ do
    let viewSize = getViewSize l
    (_, sizeX) <- getmaxyx window

    let currentPosition = getPosition l
    let viewPosition    = getViewPosition l
    let list            = take viewSize $ drop viewPosition $ getList l

    let putLine (y, element) = mvwaddnstr window y 0 (renderOne element) sizeX
    mapM_ putLine $ zip [0..] list

    let relativePosition = currentPosition - viewPosition
    mvwchgat window relativePosition 0 (-1) [Reverse] MainColor
    return ()

  wrefresh window
  return ()
