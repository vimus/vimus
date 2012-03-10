{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
import           Test.Framework.TH
import           Test.Framework.Providers.QuickCheck2

import           Test.QuickCheck
import           ListWidget hiding (null)

main = $(defaultMainGenerator)

instance Arbitrary a => Arbitrary (ListWidget a) where
  arbitrary = do
    viewSize <- choose (0, 200)
    list <- arbitrary
    position <- choose (0, (length list) - 1)
    viewPosition <- choose (max 0 (position - viewSize + 1), position)
    return $ setViewPosition (setPosition (new list viewSize) position) viewPosition

prop_invariants l
  | null list = 1 <= viewSize && position == 0 && listLength == 0 && viewPosition == 0
  | otherwise = and [
    1 <= viewSize
  , 0 <= position     && position     < listLength
  , 0 <= viewPosition && viewPosition < listLength
  , viewPosition <= position && position < viewPosition + viewSize
  ]
  where
    position      = getPosition l
    list          = getList l :: [()]
    listLength    = getListLength l
    viewSize      = getViewSize l
    viewPosition  = getViewPosition l

prop_setPosition l n = prop_invariants l_ && getPosition l_ == clamp 0 listLength n
  where
    l_          = setPosition l n
    listLength  = getListLength l_

prop_setViewPosition l n = prop_invariants l_ && getViewPosition l_ == clamp 0 listLength n
  where
    l_          = setViewPosition l n
    listLength  = getListLength l_

prop_setTotalSize l n = prop_invariants l_ && getTotalSize l_ == max 2 n
  where
    l_ = setTotalSize l n

prop_update widget l = prop_invariants $ update widget l

prop_confine lower upper n_
  | upper <= lower  = n == lower
  | otherwise       = lower <= n && n < upper
  where
    n = clamp lower upper n_
