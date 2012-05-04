{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module ListWidget (
  ListWidget

, new
, render
, setMarked

-- * current element
, getPosition
, select

-- * movement
, moveUp
, moveDown
, moveLast
, moveFirst
, scrollUp
, scrollDown
, scrollPageUp
, scrollPageDown

-- * search
, filter
, search
, searchBackward

-- * parent and children
, newChild
, getParent

-- * update
, update
, resize

#ifdef TEST

, getSize
, getViewSize
, getViewPosition

#endif
) where

import           Prelude hiding (filter)
import qualified Prelude

import           Control.Monad (when)
import           Data.Foldable (forM_)
import           Data.Default

import           Type
import           WindowLayout
import           Util (clamp)
import           Ruler
import           Render hiding (getWindowSize)

data ListWidget a = ListWidget {
  getPosition     :: Int        -- ^ Cursor position
, getList         :: [a]
, getMarked       :: Maybe Int  -- ^ Marked element
, getSize   :: Int

, getWindowSize         :: WindowSize

-- | position of viewport within the list
, getViewPosition :: Int

, getParent       :: Maybe (ListWidget a)
} deriving (Eq, Show)


-- | The number of lines that are available for content.
getViewSize :: ListWidget a -> Int
getViewSize = windowSizeY . getWindowSize

new :: [a] -> ListWidget a
new list = widget
  where
    widget = ListWidget {
        getPosition = 0
      , getList = list
      , getMarked = Nothing
      , getSize = length list
      , getWindowSize = def
      , getViewPosition = 0
      , getParent = Nothing
      }

newChild :: [a] -> ListWidget a -> ListWidget a
newChild list parent = widget {getParent = Just parent}
  where
    widget = resize (new list) (getWindowSize parent)

resize :: ListWidget a -> WindowSize -> ListWidget a
resize widget size = result
  where
    w = widget {getWindowSize = size {windowSizeY = max (windowSizeY size) 2}}
    -- to make sure that viewPosition is correct, we simply set position
    result = setPosition w $ getPosition w


update :: ListWidget a -> [a] -> ListWidget a
update widget list = setPosition newWidget $ getPosition widget
  where
    newWidget       = widget { getList = list, getSize = length list }

------------------------------------------------------------------------
-- search

filter :: (a -> Bool) -> ListWidget a -> ListWidget a
filter predicate widget = (update widget $ Prelude.filter predicate $ getList widget) `setPosition` 0

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
    matches = Prelude.filter predicate_ list
      where
        predicate_ (_, y) = predicate y

------------------------------------------------------------------------
-- move

setPosition :: ListWidget a -> Int -> ListWidget a
setPosition widget pos = widget { getPosition = newPosition, getViewPosition = newViewPosition }
  where
    newPosition     = clamp 0 listLength pos
    listLength      = getSize widget
    viewPosition    = getViewPosition widget
    minViewPosition = newPosition - (getViewSize widget - 1)
    newViewPosition = max minViewPosition $ min viewPosition newPosition

moveFirst :: ListWidget a -> ListWidget a
moveFirst l = setPosition l 0

moveLast :: ListWidget a -> ListWidget a
moveLast l = setPosition l $ getSize l - 1

moveUp :: ListWidget a -> ListWidget a
moveUp l = setPosition l (getPosition l - 1)

moveDown :: ListWidget a -> ListWidget a
moveDown l = setPosition l (getPosition l + 1)

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
    newViewPosition = min (max 0 $ getSize l - 1) $ getViewPosition l + n

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
  if getSize l > 0
    then Just $ getList l !! getPosition l
    else Nothing

setMarked :: ListWidget a -> Maybe Int -> ListWidget a
setMarked w x = w { getMarked = x }

render :: (Renderable a) => ListWidget a -> Render Ruler
render l = do
  let listLength      = getSize l
      viewSize        = getViewSize l
      viewPosition    = getViewPosition l
      currentPosition = getPosition l

  when (listLength > 0) $ do

    let list            = take viewSize $ drop viewPosition $ getList l

    let putLine (y, element) = addstr y 0 (renderItem element)
    mapM_ putLine $ zip [0..] list

    let cursorPosition = currentPosition - viewPosition
    chgat cursorPosition [Reverse] MainColor

    forM_ (getMarked l) $ \marked -> do
      let y = marked - viewPosition
      when (0 <= y && y < viewSize) $ do
        let attr = if y == cursorPosition then [Bold, Reverse] else [Bold]
        chgat y attr MainColor

  let positionIndicator
        | listLength > 0 = Just (succ currentPosition, listLength)
        | otherwise      = Nothing

  return $ Ruler rulerText positionIndicator (visible listLength viewSize viewPosition)

  where
    rulerText = maybe "" breadcrumbs (getParent l)
    breadcrumbs list = case getParent list of
      Just p  -> breadcrumbs p ++ " > " ++ this
      Nothing -> this
      where
        this = maybe "" renderItem (select list)
