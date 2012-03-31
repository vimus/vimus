{-# LANGUAGE DeriveFunctor #-}
module Data.List.Zipper where

data ListZipper a = ListZipper ![a] ![a]
  deriving Functor

empty :: ListZipper a
empty = ListZipper [] []

isEmpty :: ListZipper a -> Bool
isEmpty (ListZipper [] []) = True
isEmpty _                  = False

{-
atEnd :: ListZipper a -> Bool
atEnd (ListZipper _ []) = True
atEnd _ = False

atStart :: ListZipper a -> Bool
atStart (ListZipper [] _) = True
atStart _ = False
-}

insertLeft :: a -> ListZipper a -> ListZipper a
insertLeft x (ListZipper xs ys) = ListZipper (x:xs) ys

{-
takeLeft :: ListZipper a -> Maybe a
takeLeft (ListZipper (x:_) _) = Just x
takeLeft _ = Nothing

takeRight :: ListZipper a -> Maybe a
takeRight (ListZipper _ (y:_)) = Just y
takeRight _ = Nothing
-}

dropLeft :: ListZipper a -> ListZipper a
dropLeft (ListZipper (_:xs) ys) = ListZipper xs ys
dropLeft s = s

dropRight :: ListZipper a -> ListZipper a
dropRight (ListZipper xs (_:ys)) = ListZipper xs ys
dropRight s = s

goLeft :: ListZipper a -> ListZipper a
goLeft (ListZipper (x:xs) ys) = ListZipper xs (x:ys)
goLeft s = s

goRight :: ListZipper a -> ListZipper a
goRight (ListZipper xs (y:ys)) = ListZipper (y:xs) ys
goRight s = s

goFirst :: ListZipper a -> ListZipper a
goFirst (ListZipper xs ys) = ListZipper [] (reverse xs ++ ys)

goLast :: ListZipper a -> ListZipper a
goLast (ListZipper xs ys) = ListZipper (reverse ys ++ xs) []

toList :: ListZipper a -> [a]
toList (ListZipper prev next) = reverse prev ++ next

-- | Create a zipper from given list, set focus at start.
fromList :: [a] -> ListZipper a
fromList s = ListZipper [] s
