{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TabSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.QuickCheck

import           Prelude hiding (mapM)
import           Data.Traversable
import           Control.Applicative

import           Data.Functor.Identity
import           Control.Monad.Trans.Writer

import           Tab

instance Arbitrary TabName where
  arbitrary = elements [Playlist, Library, Browser, SearchResult]

instance (Arbitrary a) => Arbitrary (Tab a) where
  arbitrary = Tab <$> arbitrary <*> arbitrary <*> pure Persistent

instance (Arbitrary a) => Arbitrary (Tabs a) where
  arbitrary = Tabs <$> arbitrary <*> arbitrary <*> arbitrary

deriving instance (Eq a) => Eq (Tab a)
deriving instance (Eq a) => Eq (Tabs a)
deriving instance (Show a) => Show (Tab a)
deriving instance (Show a) => Show (Tabs a)

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "traverse" $ do
    prop "evaluates from left to right" $
      \(tabs :: Tabs Int) -> execWriter (mapM (\x -> tell [x]) tabs) == map (\(Tab _ c _) -> c) (toList tabs)

    prop "collects the results in order" $
      \(tabs :: Tabs Int) -> runIdentity (traverse return tabs) == tabs

  describe "previous" $ do
    prop "is inverse to next" $ do
      \(tabs :: Tabs Int) -> (previous . next) tabs == tabs

    prop "regards auto-close" $
      \(tabs :: Tabs Int) tab -> previous (insert tab {tabCloseMode = AutoClose} tabs) == tabs

  describe "next" $ do
    prop "is inverse to previous" $
      \(tabs :: Tabs Int) -> (next . previous) tabs == tabs

    prop "regards auto-close" $
      \(tabs :: Tabs Int) tab -> next (insert tab {tabCloseMode = AutoClose} tabs) == next tabs

  describe "insert" $ do
    prop "regards auto-close" $ do
      \tabs t1 t2 -> insert t2 (insert t1 {tabCloseMode = AutoClose} tabs) == insert t2 (tabs :: Tabs Int)

  describe "select" $ do
    prop "regards auto-close" $ \(tabs :: Tabs Int) tab -> do
      name <- tabName <$> elements (toList tabs)
      return $ select name (insert tab {tabCloseMode = AutoClose} tabs) == select name tabs
