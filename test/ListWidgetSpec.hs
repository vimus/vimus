{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ListWidgetSpec (main, spec) where

import           Control.Applicative
import           Test.Hspec.ShouldBe
import           Test.QuickCheck

import           ListWidget
import           Type

-- TODO:
--
-- * resize to modifications
-- * add update to modifications

instance Arbitrary WindowSize where
  arbitrary = WindowSize <$> choose (0, 100)  <*> choose (0, 200)

newtype Widget = Widget (ListWidget ())
  deriving (Eq, Show)

-- |
-- An arbitrary widget with one or more elements, and arbitrary modifications
-- applied.
instance Arbitrary Widget where
  arbitrary = Widget <$> do
    widget <- nonEmptyWidget

    modifications <- listOf movement

    return (apply modifications widget)
    where
      nonEmptyWidget = do
        NonEmpty list <- arbitrary
        return (new list)

      apply :: [ListWidget a -> ListWidget a] -> ListWidget a -> ListWidget a
      apply = foldr (.) id

      movement = elements [
          moveUp
        , moveDown
        , moveLast
        , moveFirst
        , scrollUp
        , scrollDown
        , scrollPageUp
        , scrollPageDown
        ]

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  let context = describe
      always  = prop

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

  describe "ListWidget" $ do

    context "with one or more elements, and arbitrary modification applied" $ do

      describe "position" $ do
        always "is >= 0" $
          \(Widget w) -> getPosition w >= 0

        always "is < size" $
          \(Widget w) -> getPosition w < getSize w

        always "is >= view position" $
          \(Widget w) -> getPosition w >= getViewPosition w

        always "is < (viwe position + view size)" $
          \(Widget w) -> getPosition w < (getViewPosition w + getViewSize w)

      describe "view position" $ do
        always "is >= 0" $
          \(Widget w) -> 0 <= getViewPosition w

        always "is < size" $
          \(Widget w) -> getViewPosition w < getSize w
