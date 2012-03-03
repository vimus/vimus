module Tab (
  Tab (..)
, TabName (..)
, Tabs (..)

-- * Conversion to and from lists
, fromList
, toList

, hasTab
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
import           Data.Foldable hiding (toList)

-- | Tab zipper
data Tabs a = Tabs ![Tab a] ![Tab a]

data Tab a = Tab {
  tabName    :: !TabName
, tabContent :: !a
}

instance Functor Tab where
  fmap f (Tab n c) = Tab n (f c)

instance Functor Tabs where
  fmap f (Tabs prev next) = Tabs (map g prev) (map g next)
    where g = fmap f

instance Foldable Tabs where
  foldr f z (Tabs prev next) = foldl' (flip g) (foldr g z next) prev
    where g (Tab _ c) = f c

instance Traversable Tabs where
  traverse f (Tabs prev next) = Tabs <$> (reverse <$> traverse g (reverse prev)) <*> traverse g next
    where g (Tab n c) = Tab n <$> f c

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
fromList = Tabs []

toList :: Tabs a -> [Tab a]
toList (Tabs prev next) = reverse prev ++ next

tabNext :: Tabs a -> Tabs a
tabNext (Tabs prev next) = case next of
  [this]    -> Tabs [] (reverse $ this:prev)
  this:rest -> Tabs (this:prev) rest
  _         -> error "No tabs!"

tabPrev :: Tabs a -> Tabs a
tabPrev (Tabs prev next) = case prev of
  this:rest -> Tabs rest (this:next)
  []        -> Tabs (tail list) [head list]
                where list = reverse next

currentTab :: Tabs a -> Tab a
currentTab (Tabs _ next) = case next of
  this:_ -> this
  []     -> error "No tabs!"

-- Sanity check function, useful if we ever decide to change tabName to String
-- instead of TabName
hasTab :: Tabs a -> TabName -> Bool
hasTab (Tabs prev next) v = prev `has` v || next `has` v
  where
    has :: [Tab a] -> TabName -> Bool
    has []     _ = False
    has (x:xs) y = (tabName x == y) || xs `has` y

selectTab :: TabName -> Tabs a -> Tabs a
selectTab v tv = case tv `hasTab` v of
  True  -> Tabs (reverse prev) next
            where (prev, next) = break ((== v) . tabName) (toList tv)
  False -> tv

modify :: (Tab a -> Tab a) -> Tabs a -> Tabs a
modify f (Tabs prev (this:rest)) = Tabs prev (f this : rest)
modify _ _ = error "No tabs!"

-- FIXME: this inserts before the current tab, but it should probably insert
-- after..
insert :: Tab a -> Tabs a -> Tabs a
insert tab (Tabs prev next) = Tabs prev (tab:next)
