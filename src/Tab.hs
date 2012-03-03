module Tab (
  Tab (..)
, TabName (..)
, Tabs (..)

, fromList
, toList

, previous
, next
, select

, current
, modify
, insert
) where

import           Prelude hiding (foldl, foldr)
import           Control.Applicative
import           Data.Traversable
import           Data.Foldable hiding (any, toList)

-- | Tab zipper
data Tabs a = Tabs ![Tab a] !(Tab a) ![Tab a]

data Tab a = Tab {
  tabName    :: !TabName
, tabContent :: !a
}

instance Functor Tab where
  fmap f (Tab n c) = Tab n (f c)

instance Functor Tabs where
  fmap g (Tabs xs c ys) = Tabs (map f xs) (f c) (map f ys)
    where f = fmap g

instance Foldable Tabs where
  foldr g z (Tabs xs y ys) = foldl' (flip f) (foldr f z (y:ys)) xs
    where f (Tab _ c) = g c

instance Traversable Tabs where
  traverse g (Tabs xs y ys) = Tabs <$> (reverse <$> traverse f (reverse xs)) <*> f y <*> traverse f ys
    where f (Tab n c) = Tab n <$> g c

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
previous (Tabs (x:xs) c ys) = Tabs xs x (c:ys)
previous (Tabs []     c ys) =
  case reverse (c:ys) of
    x:xs -> Tabs xs x []
    []   -> undefined

-- | Move focus to the right.
next :: Tabs a -> Tabs a
next (Tabs xs c (y:ys)) = Tabs (c:xs) y ys
next (Tabs xs c    [] ) =
  case reverse (c:xs) of
    y:ys -> Tabs [] y ys
    []   -> undefined

-- | Set focus to first tab with given name.
select :: TabName -> Tabs a -> Tabs a
select name tabs =
  case break ((== name) . tabName) (toList tabs) of
    (xs, y:ys) -> Tabs (reverse xs) y ys
    _          -> tabs

-- | Return the focused tab.
current :: Tabs a -> Tab a
current (Tabs _ c _) = c

-- | Modify the focused tab.
modify :: (Tab a -> Tab a) -> Tabs a -> Tabs a
modify f (Tabs xs c ys) = Tabs xs (f c) ys

-- | Insert a new tab after the focused tab; set focus to the new tab.
insert :: Tab a -> Tabs a -> Tabs a
insert x (Tabs xs c ys) = Tabs (c:xs) x ys
