{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Widget.ListWidgetSpec (main, spec) where

import           Control.Applicative
import           Test.Hspec
import           Test.QuickCheck

import           Widget.Type
import           Widget.ListWidget hiding (resize)
import qualified Widget.ListWidget as ListWidget

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

    actions <- listOf $ frequency [(20, movement), (1, other)]

    return (apply actions widget)
    where
      nonEmptyWidget = do
        NonEmpty list <- arbitrary
        return (new list)

      apply :: [ListWidget a -> ListWidget a] -> ListWidget a -> ListWidget a
      apply = foldr (.) id

      other = oneof [update_, resize_]

      update_ = do
        NonEmpty list <- arbitrary
        return (\w -> update (==) w list)

      resize_ = flip ListWidget.resize <$> arbitrary

      movement = oneof [
          pure moveUp
        , pure moveDown
        , pure moveLast
        , pure moveFirst
        , scroll <$> choose (-23, 23)
        ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let always s = it s . property

  describe "A ListWidget" $ do
    context "with some elements" $ do
      let someWidget = new [0 .. 100 :: Int]

      describe "new" $ do
        it "sets the initial view position to 0" $ do
          getViewPosition someWidget `shouldBe` 0

      describe "scroll" $ do
        it "moves the view by a given offset" $ do
          (getViewPosition . scroll 20) someWidget `shouldBe` 20

        it "works for a negative offset" $ do
          (getViewPosition . scroll (-5) . scroll 20) someWidget `shouldBe` 15

    context "with one or more elements, and arbitrary modifications applied" $ do
      describe "position" $ do
        always "is >= 0" $
          \(Widget w) -> getPosition w >= 0

        always "is < length" $
          \(Widget w) -> getPosition w < getLength w

        always "is >= view position" $
          \(Widget w) -> getPosition w >= getViewPosition w

        always "is < (viwe position + view size)" $
          \(Widget w) -> getPosition w < (getViewPosition w + getViewSize w)

      describe "view position" $ do
        always "is >= 0" $
          \(Widget w) -> 0 <= getViewPosition w

        always "is < length" $
          \(Widget w) -> getViewPosition w < getLength w

  describe "moveTo" $ do
    it "selects a given element" $ do
      let l = new [10 .. 42 :: Int]
      (moveTo 23 l >>= select) `shouldBe` Just 23

  describe "breadcrumbs" $ do
    it "returns path to current element" $ do
      let l = (moveDown . moveDown . moveDown . newChild [0, 100 .. 500])
            $ (moveDown . newChild [0, 10 .. 50])
            $ (moveDown . moveDown . new) [0 .. 5 :: Int]
      breadcrumbs l `shouldBe` [2, 10, 300]

  describe "update" $ do
    it "keeps focus on current element, if possible" $ do
      let l = setPosition (new [0..10 :: Int]) 5
      getPosition (update (==) l [3..8]) `shouldBe` 2

    it "alternatively sets focus to next element that was in the original list" $ do
      let l = setPosition (new [0..10 :: Int]) 5
      getPosition (update (==) l ([0..2] ++ [8..10])) `shouldBe` 3

    it "alternatively sets focus to previous element that was in the original list" $ do
      let l = setPosition (new [0..10 :: Int]) 5
      getPosition (update (==) l ([0..2] ++ [30 .. 40])) `shouldBe` 2

    it "alternatively sets focus to first element" $ do
      let l = setPosition (new [0..10 :: Int]) 5
      getPosition (update (==) l [30 .. 40]) `shouldBe` 0

  describe "moveUpWhile" $ do
    let l = setPosition (new [0,0,0,3,3,3,6,6,6,9,9,9 :: Int]) 6
    it "moves to previous element that does still satisfy predicate" $ do
      getPosition (moveUpWhile (== 3) $ l) `shouldBe` 3

    it "moves to first element, if all previous elements satisfy predicate" $ do
      getPosition (moveUpWhile (== 0) . moveUpWhile (== 3) $ l) `shouldBe` 0

  describe "moveDownWhile" $ do
    let l = setPosition (new [0,0,0,3,3,3,6,6,6,9,9,9 :: Int]) 6
    it "moves to next element that does not satisfy predicate" $ do
      getPosition (moveDownWhile (== 6) l) `shouldBe` 9

    it "moves to last element, if all following elements satisfy predicate" $ do
      getPosition (moveDownWhile (==9) . moveDownWhile (== 6) $ l) `shouldBe` 11
