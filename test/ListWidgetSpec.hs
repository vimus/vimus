{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ListWidgetSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.QuickCheck

import           ListWidget hiding (null)
import           Util (clamp)

instance Arbitrary a => Arbitrary (ListWidget a) where
  arbitrary = do
    viewSize <- choose (0, 200)
    list <- arbitrary
    position <- choose (0, (length list) - 1)
    viewPosition <- choose (max 0 (position - viewSize + 1), position)
    return $ setViewPosition (setPosition (new list viewSize) position) viewPosition

deriving instance Eq Visible

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "visible (an internal function)" $ do
    it "All" $ do
      visible 20 20 0 `shouldBe` All

    it "Top - Bot" $ do
      visible 20 19 0 `shouldBe` Top
      visible 20 19 1 `shouldBe` Bot

    it "Top - 50 - Bot" $ do
      visible 20 18 0 `shouldBe` Top
      visible 20 18 1 `shouldBe` Percent 50
      visible 20 18 2 `shouldBe` Bot

    it "Top - 33 - 66 - Bot" $ do
      visible 20 17 0 `shouldBe` Top
      visible 20 17 1 `shouldBe` Percent 33
      visible 20 17 2 `shouldBe` Percent 66
      visible 20 17 3 `shouldBe` Bot

    it "Top - 25 - 50 - 75 - Bot" $ do
      visible 20 16 0 `shouldBe` Top
      visible 20 16 1 `shouldBe` Percent 25
      visible 20 16 2 `shouldBe` Percent 50
      visible 20 16 3 `shouldBe` Percent 75
      visible 20 16 4 `shouldBe` Bot

  describe "properties" $ do
    prop "prop_invariants" prop_invariants
    prop "prop_setPosition" prop_setPosition
    prop "prop_setViewPosition" prop_setViewPosition
    prop "prop_setTotalSize" prop_setTotalSize
    prop "prop_update" prop_update

  where
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
