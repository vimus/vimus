{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Vimus.Widget.ListWidget (
  ListWidget
, new
, getLength

-- * current element
, getPosition
, select
, breadcrumbs

, selected
, removeSelected
, selectGroupBy

-- * movement
, move
, moveUp
, moveDown
, moveUpWhile
, moveDownWhile
, moveLast
, moveFirst

, moveTo
, setPosition
, noVisual

-- * parent and children
, newChild
, getParent

-- * update
, update
, append
, resize

-- * format
, getElementsFormat
, setElementsFormat

-- * exported, because they have fewer constraints than the Widget variants
, Vimus.Widget.ListWidget.render
, Vimus.Widget.ListWidget.searchItem
, Vimus.Widget.ListWidget.filterItem
, Vimus.Widget.ListWidget.handleEvent

-- * exported for testing
, getElements
, getViewSize
, getViewPosition
, getVisualStart
, scroll
) where

import           Data.List (isInfixOf, intercalate, findIndex, find, groupBy)
import           Data.Maybe
import           Data.Char (toLower)
import           Data.Function (on)

import           Control.Monad (when)
import           Data.Foldable (forM_, asum)
import           Data.Default

import           Vimus.Widget.Type
import           Vimus.WindowLayout
import           Vimus.Util (clamp)
import           Vimus.Ruler
import           Vimus.Render hiding (getWindowSize)
import           Vimus.Type (Widget(..), Event(..), SearchOrder(..))
import           Content

data ListWidget f a = ListWidget {
  getPosition     :: Int        -- ^ Cursor position
, getElements     :: [a]
, getFormat       :: f
, getLength       :: Int
, getVisualStart  :: Maybe Int  -- ^ First element of visual selection
, windowSize      :: WindowSize

-- | position of viewport within the list
, getViewPosition :: Int

, getListParent   :: Maybe (ListWidget f a)
} deriving (Eq, Show, Functor)

instance Default f => Default (ListWidget f a) where
  def = ListWidget def def def def def def def def

instance (f ~ Format a, Searchable a, Renderable a) => Widget (ListWidget f a) where
  render      = Vimus.Widget.ListWidget.render (const False)
  currentItem = const Nothing
  searchItem  = Vimus.Widget.ListWidget.searchItem
  filterItem  = Vimus.Widget.ListWidget.filterItem
  handleEvent = Vimus.Widget.ListWidget.handleEvent

handleEvent :: Monad m => ListWidget f a -> Event -> m (ListWidget f a)
handleEvent l@ListWidget{..} ev = return $ case ev of
  EvMoveUp         -> moveUp l
  EvMoveDown       -> moveDown l
  EvMoveFirst      -> moveFirst l
  EvMoveLast       -> moveLast l
  EvScroll n       -> scroll n l
  EvResize size    -> resize l size
  EvVisual         -> if 0 < getLength then l {getVisualStart = Just getPosition} else l
  EvNoVisual       -> noVisual True l
  _                -> l

noVisual :: Bool -> ListWidget f a -> ListWidget f a
noVisual keepPosition l@ListWidget{..}
  | keepPosition = l {getVisualStart = Nothing}
  | otherwise    = setPosition l {getVisualStart = Nothing} (fromMaybe getPosition getVisualStart)

-- | The number of lines that are available for content.
getViewSize :: ListWidget f a -> Int
getViewSize = windowSizeY . windowSize

getParent :: ListWidget f a -> Maybe (ListWidget f a)
getParent list = fmap (setElementsFormat (getFormat list)) (getListParent list)

new :: Default f => [a] -> ListWidget f a
new = setElements def

newChild :: forall f a. Default f => [a] -> ListWidget f a -> ListWidget f a
newChild list parent = widget {getListParent = Just parent, getFormat = getFormat parent}
  where
    widget :: ListWidget f a
    widget = resize (new list) (windowSize parent)

resize :: ListWidget f a -> WindowSize -> ListWidget f a
resize l@ListWidget{..} size = sanitize $ l {
    windowSize = sanitizeWindowSize size
  , getListParent = (`resize` size) `fmap` getListParent
  }
  where
    -- make sure that the window height is never < 2
    sanitizeWindowSize :: WindowSize -> WindowSize
    sanitizeWindowSize s@WindowSize{..} = s {windowSizeY = max windowSizeY 2}

-- | Make sure that position is within the viewport.
--
-- The viewport is moved, if necessary.
sanitize :: ListWidget f a -> ListWidget f a
sanitize l@ListWidget{..} = setPosition l getPosition

update :: (a -> a -> Bool) -> ListWidget f a -> [a] -> ListWidget f a
update eq widget@ListWidget{..} list = setPosition (setElements widget list) (fromMaybe 0 mNewPos)
  where
    mNewPos = asum $ ys ++ reverse xs
      where
        (xs, ys) = splitAt getPosition $ map ((`findIndex` list) . eq) getElements

-- IMPORTANT: You must call `setPosition` after `setElements`!
setElements :: ListWidget f a -> [a] -> ListWidget f a
setElements widget list = widget {getElements = list, getLength = length list, getListParent = Nothing}

getElementsFormat :: ListWidget f a -> f
getElementsFormat list = getFormat list

setElementsFormat :: f -> ListWidget f a -> ListWidget f a
setElementsFormat format list = list { getFormat = format }

append :: ListWidget f a -> a -> ListWidget f a
append widget@(ListWidget{..}) x = widget{getElements = ys, getLength = length ys}
  where
    ys = getElements ++ [x]

------------------------------------------------------------------------
-- search


filterItem :: Searchable a => ListWidget f a -> String -> ListWidget f a
filterItem w t = filter_ (filterPredicate t) w
  where
    filter_ :: (a -> Bool) -> ListWidget f a -> ListWidget f a
    filter_ predicate widget = (setElements widget $ filter predicate $ getElements widget) `setPosition` 0

-- | Rotate elements of given list by given number.
--
-- >>> rotate 3 [0..10]
-- [3,4,5,6,7,8,9,10,0,1,2]
rotate :: Int -> [a] -> [a]
rotate n l = drop n l ++ take n l

searchForward :: (a -> Bool) -> ListWidget f a -> ListWidget f a
searchForward predicate widget = maybe widget (setPosition widget) match
  where
    match = findFirst predicate shiftedList
    -- rotate list, to get next match from current position
    shiftedList = rotate n enumeratedList
      where
        n = getPosition widget + 1
        enumeratedList = zip [0..] $ getElements widget

searchBackward :: (a -> Bool) -> ListWidget f a -> ListWidget f a
searchBackward predicate widget = maybe widget (setPosition widget) match
  where
    match = findFirst predicate shiftedList
    -- rotate list, to get next match from current position
    shiftedList = reverse $ rotate n enumeratedList
      where
        n = getPosition widget
        enumeratedList = zip [0..] $ getElements widget

findFirst :: (a -> Bool) -> [(Int, a)] -> Maybe Int
findFirst predicate list = case matches of
  (n, _):_  -> Just n
  _         -> Nothing
  where
    matches = filter predicate_ list
      where
        predicate_ (_, y) = predicate y

searchItem :: Searchable a => ListWidget f a -> SearchOrder -> String -> ListWidget f a
searchItem w Forward  t = searchForward  (searchPredicate t) w
searchItem w Backward t = searchBackward (searchPredicate t) w

-- | Select given element.
moveTo :: Eq a => a -> ListWidget f a -> Maybe (ListWidget f a)
moveTo c lw = setPosition lw `fmap` findFirst (==c) (zip [0..] $ getElements lw)

data SearchPredicate = Search | Filter

searchPredicate :: Searchable a => String -> a -> Bool
searchPredicate = searchPredicate_ Search

filterPredicate :: Searchable a => String -> a -> Bool
filterPredicate = searchPredicate_ Filter

searchPredicate_ :: Searchable a => SearchPredicate -> String -> a -> Bool
searchPredicate_ predicate "" _ = onEmptyTerm predicate
  where
    onEmptyTerm Search = False
    onEmptyTerm Filter = True
searchPredicate_ _ term item = all (\term_ -> any (isInfixOf term_) tags) terms
  where
    tags = map (map toLower) (searchTags item)
    terms = words $ map toLower term

------------------------------------------------------------------------
-- move

setPosition :: ListWidget f a -> Int -> ListWidget f a
setPosition widget pos = widget { getPosition = newPosition, getViewPosition = newViewPosition }
  where
    newPosition     = clamp 0 listLength pos
    listLength      = getLength widget
    viewPosition    = getViewPosition widget
    minViewPosition = newPosition - (getViewSize widget - 1)
    newViewPosition = max minViewPosition $ min viewPosition newPosition

moveFirst :: ListWidget f a -> ListWidget f a
moveFirst l = setPosition l 0

moveLast :: ListWidget f a -> ListWidget f a
moveLast l = setPosition l (getLength l - 1)

moveUp :: ListWidget f a -> ListWidget f a
moveUp = move (-1)

moveDown :: ListWidget f a -> ListWidget f a
moveDown = move 1

move :: Int -> ListWidget f a -> ListWidget f a
move n l = setPosition l (getPosition l + n)

moveUpWhile :: (a -> Bool) -> ListWidget f a -> ListWidget f a
moveUpWhile p l@ListWidget{..} = setPosition l pos
  where
    pos = getPosition - (length . takeWhile p . reverse . take getPosition) getElements

moveDownWhile :: (a -> Bool) -> ListWidget f a -> ListWidget f a
moveDownWhile p l@ListWidget{..} = setPosition l pos
  where
    pos = getPosition + (length . takeWhile p . drop getPosition) getElements

scroll :: Int -> ListWidget f a -> ListWidget f a
scroll n l = l {getViewPosition = newViewPosition, getPosition = newPosition}
  where
    newViewPosition = clamp 0 (getLength l) (getViewPosition l + n)
    newPosition     = clamp newViewPosition (newViewPosition + getViewSize l) (getPosition l)

select :: ListWidget f a -> Maybe a
select l
  | getLength l == 0 = Nothing
  | otherwise        = Just (getElements l !! getPosition l)

selected :: ListWidget f a -> [a]
selected ListWidget{..}
  | getLength == 0 = []
  | otherwise = take n $ drop a $ getElements
  where
    start = fromMaybe getPosition getVisualStart
    a = min getPosition start
    b = max getPosition start
    n = succ (b - a)

removeSelected :: ListWidget f a -> ListWidget f a
removeSelected l@ListWidget{..}
  | getLength == 0 = l
  | otherwise = (setElements l (take a getElements ++ drop b getElements) `setPosition` a) {getVisualStart = Nothing}
  where
    start = fromMaybe getPosition getVisualStart
    a = min getPosition start
    b = succ $ max getPosition start

-- | Selects all elements surrounding the cursor that satisfy the equality
-- predicate (like `groupBy`).
selectGroupBy :: (a -> a -> Bool) -> ListWidget f a -> ListWidget f a
selectGroupBy f l@ListWidget{..}
  | getLength == 0 = l
  | otherwise = case find (getPosition `elem`) allGroups of
    Just curGroup   -> l { getVisualStart = Just $ head curGroup
                         , getPosition    = last curGroup }
    Nothing         -> l
  where
    allGroups = map (map fst) . groupBy (f `on` snd) $ zip [0..] getElements

render :: (f ~ Format a, Renderable a) => (a -> Bool) -> ListWidget f a -> Render Ruler
render isMarked l = do
  let listLength      = getLength l
      viewSize        = getViewSize l
      viewPosition    = getViewPosition l
      currentPosition = getPosition l
      format          = getElementsFormat l
      visualStart     = fromMaybe currentPosition $ getVisualStart l

  when (listLength > 0) $ do

    let isSelected y = a <= y && y <= b
        a = clamp 0 viewSize $ min currentPosition visualStart - viewPosition
        b = clamp 0 viewSize $ max currentPosition visualStart - viewPosition

        list            = take viewSize $ drop viewPosition $ getElements l

    forM_ (zip [0..] list) $ \(y, element) -> do
      addLine y 0 (renderItem format element)
      case (isMarked element, isSelected y) of
        (True,  True ) -> chgat y [Reverse, Bold] MainColor
        (True,  False) -> chgat y [Bold] MainColor
        (False, True ) -> chgat y [Reverse] MainColor
        (False, False) -> return ()

  let positionIndicator
        | listLength > 0 = Just (succ currentPosition, listLength)
        | otherwise      = Nothing
      rulerText = maybe "" showBreadcrumbs (getListParent l)
      showBreadcrumbs = intercalate " > " . map (toPlainText . renderItem format) . breadcrumbs

  return $ Ruler rulerText positionIndicator (visible listLength viewSize viewPosition)

-- | Return path to current element.
breadcrumbs :: ListWidget f a -> [a]
breadcrumbs = reverse . go
  where
    go l = maybe id (:) (select l) $ case getListParent l of
      Just p  -> go p
      Nothing -> []
