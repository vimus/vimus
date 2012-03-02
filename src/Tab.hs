module Tab where

-- | Tab zipper
data TabZipper a = TabZipper ![Tab a] ![Tab a]

type Tab a = (TabName, a)

data TabName = Playlist | Library | Browser | SearchResult | Temporary String
  deriving Eq

instance Show TabName where
  show view = case view of
    Playlist      -> "Playlist"
    Library       -> "Library"
    Browser       -> "Browser"
    SearchResult  -> "SearchResult"
    Temporary s   -> s

tabName :: Tab a -> TabName
tabName = fst

-- FIXME: rename
tabWidget :: Tab a -> a
tabWidget = snd


tabFromList :: [Tab a] -> TabZipper a
tabFromList = TabZipper []

tabNext :: TabZipper a -> TabZipper a
tabNext (TabZipper prev next) = case next of
  [this]    -> TabZipper [] (reverse $ this:prev)
  this:rest -> TabZipper (this:prev) rest
  _         -> error "No tabs!"

tabPrev :: TabZipper a -> TabZipper a
tabPrev (TabZipper prev next) = case prev of
  this:rest -> TabZipper rest (this:next)
  []        -> TabZipper (tail list) [head list]
                where list = reverse next

currentTab :: TabZipper a -> Tab a
currentTab (TabZipper _ next) = case next of
  this:_ -> this
  []     -> error "No tabs!"

-- Sanity check function, useful if we ever decide to change tabName to String
-- instead of TabName
hasTab :: TabZipper a -> TabName -> Bool
hasTab (TabZipper prev next) v = prev `has` v || next `has` v
  where
    has :: [Tab a] -> TabName -> Bool
    has []     _ = False
    has (x:xs) y = (tabName x == y) || xs `has` y

getTabs :: TabZipper a -> [Tab a]
getTabs (TabZipper prev next) = reverse prev ++ next

selectTab :: TabName -> TabZipper a -> TabZipper a
selectTab v tv = case tv `hasTab` v of
  True  -> TabZipper (reverse prev) next
            where (prev, next) = break ((== v) . tabName) (getTabs tv)
  False -> tv
