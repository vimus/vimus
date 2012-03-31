{-# LANGUAGE DeriveFunctor #-}
module Data.List.Pointed where

data PointedList a = PointedList ![a] !a ![a]
  deriving Functor

-- | Return the focused element.
focus :: PointedList a -> a
focus (PointedList _ x _) = x

-- | Modify the focused element.
modify :: (a -> a) -> PointedList a -> PointedList a
modify f (PointedList xs c ys) = PointedList xs (f c) ys

goLeft :: PointedList a -> PointedList a
goLeft (PointedList (x:xs) c ys) = PointedList xs x (c:ys)
goLeft s = s

goRight :: PointedList a -> PointedList a
goRight (PointedList xs c (y:ys)) = PointedList (c:xs) y ys
goRight s = s

atEnd :: PointedList a -> Bool
atEnd (PointedList _ _ []) = True
atEnd _ = False

atStart :: PointedList a -> Bool
atStart (PointedList [] _ _) = True
atStart _ = False
