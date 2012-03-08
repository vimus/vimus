module Tab (
  Tab (..)
, TabName (..)
, CloseMode (..)
, Tabs (..)

, fromList
, toList

, previous
, next
, select

, current
, modify
, insert
, close
) where

import           Prelude hiding (foldl, foldr)
import           Control.Applicative
import           Data.Traversable
import           Data.Foldable hiding (any, toList)

-- | Tab zipper
data Tabs a = Tabs ![Tab a] !(Tab a) ![Tab a]

data CloseMode =
    Persistent  -- ^ tab can not be closed
  | Closeable   -- ^ tab can be closed
  | AutoClose   -- ^ tab is automatically closed on unfocus
  deriving Eq

-- | True, if tab is automatically closed on unfocus.
isAutoClose :: Tab a -> Bool
isAutoClose = (== AutoClose) . tabCloseMode

-- | True, if tab can be closed.
isCloseable :: Tab a -> Bool
isCloseable = (/= Persistent) . tabCloseMode

data Tab a = Tab {
  tabName      :: !TabName
, tabContent   :: !a
, tabCloseMode :: !CloseMode
}

instance Functor Tab where
  fmap f (Tab n c a) = Tab n (f c) a

instance Functor Tabs where
  fmap g (Tabs xs c ys) = Tabs (map f xs) (f c) (map f ys)
    where f = fmap g

instance Foldable Tabs where
  foldr g z (Tabs xs y ys) = foldl' (flip f) (foldr f z (y:ys)) xs
    where f (Tab _ c _) = g c

instance Traversable Tabs where
  traverse g (Tabs xs y ys) = Tabs <$> (reverse <$> traverse f (reverse xs)) <*> f y <*> traverse f ys
    where f (Tab n c a) = flip (Tab n) a <$> g c

data TabName = Playlist | Library | Browser | SearchResult | Temporary String
  deriving Eq

instance Show TabName where
  show view = case view of
    Playlist      -> "Playlist"
    Library       -> "Library"
    Browser       -> "Browser"
    SearchResult  -> "SearchResult"
    Temporary s   -> s

fromList :: [Tab a] -> Tabs a
fromList (c:ys) = Tabs [] c ys
fromList []     = error "Tab.fromList: empty list"

toList :: Tabs a -> [Tab a]
toList (Tabs xs c ys) = foldl' (flip (:)) (c:ys) xs

-- | Move focus to the left.
previous :: Tabs a -> Tabs a
previous (Tabs pre c suf) = case pre of
  x:xs -> Tabs xs x ys
  []   ->
    case reverse ys of
      x:xs -> Tabs xs x []
      []   -> error "Tab.previous: no tabs"
  where ys = if isAutoClose c then suf else c:suf

-- | Move focus to the right.
next :: Tabs a -> Tabs a
next (Tabs pre c suf) = case suf of
  y:ys -> Tabs xs y ys
  []   ->
    case reverse xs of
      y:ys -> Tabs [] y ys
      []   -> error "Tab.next: no tabs"
  where xs = if isAutoClose c then pre else c:pre

-- | Set focus to first tab with given name.
select :: TabName -> Tabs a -> Tabs a
select name tabs@(Tabs pre c suf) =
  case break ((== name) . tabName) l of
    (xs, y:ys) -> Tabs (reverse xs) y ys
    _          -> tabs
  where
    l = foldl' (flip (:)) zs pre
    zs = if isAutoClose c then suf else c:suf

-- | Return the focused tab.
current :: Tabs a -> Tab a
current (Tabs _ c _) = c

-- | Close the focused tab, if possible.
close :: Tabs a -> Maybe (Tabs a)
close (Tabs pre c suf)
  | isCloseable c =
    case pre of
      x:xs -> Just (Tabs xs x suf)
      []   -> case reverse suf of
        x:xs -> Just (Tabs xs x pre)
        []   -> Nothing
  | otherwise = Nothing

-- | Modify the focused tab.
modify :: (Tab a -> Tab a) -> Tabs a -> Tabs a
modify f (Tabs xs c ys) = Tabs xs (f c) ys

-- | Insert a new tab after the focused tab; set focus to the new tab.
insert :: Tab a -> Tabs a -> Tabs a
insert x (Tabs pre c ys) = Tabs xs x ys
  where xs = if isAutoClose c then pre else c:pre
