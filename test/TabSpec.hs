{-# LANGUAGE StandaloneDeriving #-}
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
      \l -> execWriter (mapM (\x -> tell [x]) (l :: Tabs Int) ) == map (\(Tab _ c _) -> c) (toList l)

    prop "collects the results in order" $
      \l -> runIdentity (traverse return l) == (l :: Tabs Int)

  describe "previous" $ do
    prop "is inverse to next" $ do
      \l -> (previous . next) l == (l :: Tabs Int)

    prop "regards auto-close" $
      \l t -> previous (insert t {tabCloseMode = AutoClose} l) == (l :: Tabs Int)

  describe "next" $ do
    prop "is inverse to previous" $ do
      \l -> (next . previous) l == (l :: Tabs Int)

    prop "regards auto-close" $
      \l t -> next (insert t {tabCloseMode = AutoClose} l) == next (l :: Tabs Int)

  describe "insert" $ do
    prop "regards auto-close" $ do
      \l t1 t2 -> insert t2 (insert t1 {tabCloseMode = AutoClose} l) == insert t2 (l :: Tabs Int)

  describe "select" $ do
    prop "regards auto-close" $ \l t -> do
      name <- tabName <$> elements (toList l)
      return $ select name (insert t {tabCloseMode = AutoClose} l) == select name (l :: Tabs Int)
