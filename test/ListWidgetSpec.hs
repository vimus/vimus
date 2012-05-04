{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ListWidgetSpec (main, spec) where

import           Control.Applicative
import           Test.Hspec.ShouldBe
import           Test.QuickCheck

import           ListWidget hiding (resize)
import qualified ListWidget
import           Type

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
        return (`update` list)

      resize_ = flip ListWidget.resize <$> arbitrary

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
