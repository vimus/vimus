module Tab (
  Tab (..)
, TabName (..)
, Tabs (..)

-- * Conversion to and from lists
, fromList
, toList

, currentTab
, tabPrev
, tabNext
, selectTab
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
toList (Tabs xs c ys) = reverse xs ++ (c:ys)

tabNext :: Tabs a -> Tabs a
tabNext (Tabs xs c (y:ys)) = Tabs (c:xs) y ys
tabNext (Tabs xs c    [] ) =
  case reverse (c:xs) of
    y:ys -> Tabs [] y ys
    []   -> undefined

tabPrev :: Tabs a -> Tabs a
tabPrev (Tabs (x:xs) c ys) = Tabs xs x (c:ys)
tabPrev (Tabs []     c ys) =
  case reverse (c:ys) of
    x:xs -> Tabs xs x []
    []   -> undefined

currentTab :: Tabs a -> Tab a
currentTab (Tabs _ c _) = c

selectTab :: TabName -> Tabs a -> Tabs a
selectTab name tabs =
  case break ((== name) . tabName) (toList tabs) of
    (xs, y:ys) -> Tabs (reverse xs) y ys
    _          -> tabs

modify :: (Tab a -> Tab a) -> Tabs a -> Tabs a
modify f (Tabs xs c ys) = Tabs xs (f c) ys

insert :: Tab a -> Tabs a -> Tabs a
insert x (Tabs xs c ys) = Tabs (c:xs) x ys
