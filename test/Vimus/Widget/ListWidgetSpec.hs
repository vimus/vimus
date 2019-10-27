{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vimus.Widget.ListWidgetSpec (main, spec) where

import           Control.Applicative
import           Test.Hspec
import           Test.QuickCheck

import           Vimus.Widget.Type
import           Vimus.Widget.ListWidget hiding (new, resize)
import qualified Vimus.Widget.ListWidget as ListWidget

instance Arbitrary WindowSize where
  arbitrary = WindowSize <$> choose (0, 100)  <*> choose (0, 200)

newtype Widget = Widget (ListWidget () ())
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
        return (ListWidget.new list)

      apply :: [ListWidget f a -> ListWidget f a] -> ListWidget f a -> ListWidget f a
      apply = foldr (.) id

      other = oneof [update_, resize_]

      update_ = do
        NonEmpty list <- arbitrary
        return (\w -> update (==) w list)

      resize_ = flip ListWidget.resize <$> arbitrary

      movement = oneof [
          move <$> choose (-5, 5)
        , pure $ moveLast
        , pure moveFirst
        , scroll <$> choose (-23, 23)
        ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let always s = it s . property
      new :: [a] -> ListWidget () a
      new = ListWidget.new

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
      let l = (move 3 . newChild [0, 100 .. 500])
            $ (moveDown . newChild [0, 10 .. 50])
            $ (move 2 . new) [0 .. 5 :: Int]
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

  describe "selected" $ do
    let l = (setPosition (new [0..10 :: Int]) 5) {getVisualStart = Just 2}
    it "returns all selected elements" $ do
      selected l `shouldBe` [2..5]

  describe "removeSelected" $ do
    let l = (setPosition (new [0..10 :: Int]) 5) {getVisualStart = Just 2}
    it "removes all selected elements" $ do
      getElements (removeSelected l) `shouldBe` ([0..1] ++ [6..10])

    it "moves focus to the next element" $ do
      select (removeSelected l) `shouldBe` Just 6

  describe "selectGroupBy" $ do
    let l = setPosition (new [1,0,3,5,2,4,6,7,8,1,9]) 5
        eqParity a b = even a == even b
    it "selects all even elements" $ do
      selected (selectGroupBy eqParity l) `shouldBe` [2,4,6]
