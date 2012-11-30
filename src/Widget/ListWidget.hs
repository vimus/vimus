{-# LANGUAGE CPP, DeriveFunctor, RecordWildCards #-}
module Widget.ListWidget (
  ListWidget
, new

-- * current element
, getPosition
, select
, breadcrumbs

, selected
, removeSelected

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

-- * parent and children
, newChild
, getParent

-- * update
, update
, append
, resize

-- * exported, because they have fewer constraints than the Widget variants
, Widget.ListWidget.render
, Widget.ListWidget.searchItem
, Widget.ListWidget.filterItem
, Widget.ListWidget.handleEvent

#ifdef TEST

, getLength
, getElements
, getViewSize
, getViewPosition
, getVisualStart
, scroll

#endif
) where

import           Data.List (isInfixOf, intercalate, findIndex)
import           Data.Maybe
import           Data.Char (toLower)

import           Control.Monad (when)
import           Data.Foldable (forM_, asum)
import           Data.Default

import           Widget.Type
import           WindowLayout
import           Util (clamp)
import           Ruler
import           Render hiding (getWindowSize)
import           Vimus (Widget(..), Event(..), SearchOrder(..))
import           Content

data ListWidget a = ListWidget {
  getPosition     :: Int        -- ^ Cursor position
, getElements     :: [a]
, getLength       :: Int
, getVisualStart  :: Maybe Int  -- ^ First element of visual selection
, windowSize      :: WindowSize

-- | position of viewport within the list
, getViewPosition :: Int

, getParent       :: Maybe (ListWidget a)
} deriving (Eq, Show, Functor)

instance Default (ListWidget a) where
  def = ListWidget def def def def def def def

instance (Searchable a, Renderable a) => Widget (ListWidget a) where
  render      = Widget.ListWidget.render (const False)
  currentItem = const Nothing
  searchItem  = Widget.ListWidget.searchItem
  filterItem  = Widget.ListWidget.filterItem
  handleEvent = Widget.ListWidget.handleEvent

handleEvent :: Monad m => ListWidget a -> Event -> m (ListWidget a)
handleEvent l@ListWidget{..} ev = return $ case ev of
  EvMoveUp         -> moveUp l
  EvMoveDown       -> moveDown l
  EvMoveFirst      -> moveFirst l
  EvMoveLast       -> moveLast l
  EvScroll n       -> scroll n l
  EvResize size    -> resize l size
  EvVisual         -> if 0 < getLength then l {getVisualStart = Just getPosition} else l
  EvNoVisual       -> setPosition l {getVisualStart = Nothing} (fromMaybe getPosition getVisualStart)
  _                -> l

-- | The number of lines that are available for content.
getViewSize :: ListWidget a -> Int
getViewSize = windowSizeY . windowSize

new :: [a] -> ListWidget a
new = setElements def

newChild :: [a] -> ListWidget a -> ListWidget a
newChild list parent = widget {getParent = Just parent}
  where
    widget = resize (new list) (windowSize parent)

resize :: ListWidget a -> WindowSize -> ListWidget a
resize l@ListWidget{..} size = sanitize $ l {
    windowSize = sanitizeWindowSize size
  , getParent = (`resize` size) `fmap` getParent
  }
  where
    -- make sure that the window height is never < 2
    sanitizeWindowSize :: WindowSize -> WindowSize
    sanitizeWindowSize s@WindowSize{..} = s {windowSizeY = max windowSizeY 2}

-- | Make sure that position is within the viewport.
--
-- The viewport is moved, if necessary.
sanitize :: ListWidget a -> ListWidget a
sanitize l@ListWidget{..} = setPosition l getPosition

update :: (a -> a -> Bool) -> ListWidget a -> [a] -> ListWidget a
update eq widget@ListWidget{..} list = setPosition (setElements widget list) (fromMaybe 0 mNewPos)
  where
    mNewPos = asum $ ys ++ reverse xs
      where
        (xs, ys) = splitAt getPosition $ map ((`findIndex` list) . eq) getElements

-- IMPORTANT: You must call `setPosition` after `setElements`!
setElements :: ListWidget a -> [a] -> ListWidget a
setElements widget list = widget {getElements = list, getLength = length list, getParent = Nothing}

append :: ListWidget a -> a -> ListWidget a
append widget@(ListWidget{..}) x = widget{getElements = ys, getLength = length ys}
  where
    ys = getElements ++ [x]

------------------------------------------------------------------------
-- search


filterItem :: Searchable a => ListWidget a -> String -> ListWidget a
filterItem w t = filter_ (filterPredicate t) w
  where
    filter_ :: (a -> Bool) -> ListWidget a -> ListWidget a
    filter_ predicate widget = (setElements widget $ filter predicate $ getElements widget) `setPosition` 0

-- | Rotate elements of given list by given number.
--
-- >>> rotate 3 [0..10]
-- [3,4,5,6,7,8,9,10,0,1,2]
rotate :: Int -> [a] -> [a]
rotate n l = drop n l ++ take n l

searchForward :: (a -> Bool) -> ListWidget a -> ListWidget a
searchForward predicate widget = maybe widget (setPosition widget) match
  where
    match = findFirst predicate shiftedList
    -- rotate list, to get next match from current position
    shiftedList = rotate n enumeratedList
      where
        n = getPosition widget + 1
        enumeratedList = zip [0..] $ getElements widget

searchBackward :: (a -> Bool) -> ListWidget a -> ListWidget a
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

searchItem :: Searchable a => ListWidget a -> SearchOrder -> String -> ListWidget a
searchItem w Forward  t = searchForward  (searchPredicate t) w
searchItem w Backward t = searchBackward (searchPredicate t) w

-- | Select given element.
moveTo :: Eq a => a -> ListWidget a -> Maybe (ListWidget a)
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

setPosition :: ListWidget a -> Int -> ListWidget a
setPosition widget pos = widget { getPosition = newPosition, getViewPosition = newViewPosition }
  where
    newPosition     = clamp 0 listLength pos
    listLength      = getLength widget
    viewPosition    = getViewPosition widget
    minViewPosition = newPosition - (getViewSize widget - 1)
    newViewPosition = max minViewPosition $ min viewPosition newPosition

moveFirst :: ListWidget a -> ListWidget a
moveFirst l = setPosition l 0

moveLast :: ListWidget a -> ListWidget a
moveLast l = setPosition l (getLength l - 1)

moveUp :: ListWidget a -> ListWidget a
moveUp = move (-1)

moveDown :: ListWidget a -> ListWidget a
moveDown = move 1

move :: Int -> ListWidget a -> ListWidget a
move n l = setPosition l (getPosition l + n)

moveUpWhile :: (a -> Bool) -> ListWidget a -> ListWidget a
moveUpWhile p l@ListWidget{..} = setPosition l pos
  where
    pos = getPosition - (length . takeWhile p . reverse . take getPosition) getElements

moveDownWhile :: (a -> Bool) -> ListWidget a -> ListWidget a
moveDownWhile p l@ListWidget{..} = setPosition l pos
  where
    pos = getPosition + (length . takeWhile p . drop getPosition) getElements

scroll :: Int -> ListWidget a -> ListWidget a
scroll n l = l {getViewPosition = newViewPosition, getPosition = newPosition}
  where
    newViewPosition = clamp 0 (getLength l) (getViewPosition l + n)
    newPosition     = clamp newViewPosition (newViewPosition + getViewSize l) (getPosition l)

select :: ListWidget a -> Maybe a
select l
  | getLength l == 0 = Nothing
  | otherwise        = Just (getElements l !! getPosition l)

selected :: ListWidget a -> [a]
selected ListWidget{..}
  | getLength == 0 = []
  | otherwise = take n $ drop a $ getElements
  where
    start = fromMaybe getPosition getVisualStart
    a = min getPosition start
    b = max getPosition start
    n = succ (b - a)

removeSelected :: ListWidget a -> ListWidget a
removeSelected l@ListWidget{..}
  | getLength == 0 = l
  | otherwise = (setElements l (take a getElements ++ drop b getElements) `setPosition` a) {getVisualStart = Nothing}
  where
    start = fromMaybe getPosition getVisualStart
    a = min getPosition start
    b = succ $ max getPosition start

render :: (Renderable a) => (a -> Bool) -> ListWidget a -> Render Ruler
render isMarked l = do
  let listLength      = getLength l
      viewSize        = getViewSize l
      viewPosition    = getViewPosition l
      currentPosition = getPosition l
      visualStart     = fromMaybe currentPosition $ getVisualStart l

  when (listLength > 0) $ do

    let isSelected y = a <= y && y <= b
        a = clamp 0 viewSize $ min currentPosition visualStart - viewPosition
        b = clamp 0 viewSize $ max currentPosition visualStart - viewPosition

        list            = take viewSize $ drop viewPosition $ getElements l

    forM_ (zip [0..] list) $ \(y, element) -> do
      addLine y 0 (renderItem element)
      case (isMarked element, isSelected y) of
        (True,  True ) -> chgat y [Reverse, Bold] MainColor
        (True,  False) -> chgat y [Bold] MainColor
        (False, True ) -> chgat y [Reverse] MainColor
        (False, False) -> return ()

  let positionIndicator
        | listLength > 0 = Just (succ currentPosition, listLength)
        | otherwise      = Nothing

  return $ Ruler rulerText positionIndicator (visible listLength viewSize viewPosition)

  where
    rulerText = maybe "" showBreadcrumbs (getParent l)
    showBreadcrumbs = intercalate " > " . map (toPlainText . renderItem) . breadcrumbs

-- | Return path to current element.
breadcrumbs :: ListWidget a -> [a]
breadcrumbs = reverse . go
  where
    go l = maybe id (:) (select l) $ case getParent l of
      Just p  -> go p
      Nothing -> []
