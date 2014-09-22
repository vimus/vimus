{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vimus.TabSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Prelude hiding (mapM)
import           Data.Traversable
import           Control.Applicative

import           Data.Functor.Identity
import           Control.Monad.Trans.Writer

import           Vimus.Tab

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
main = hspec spec

spec :: Spec
spec = do

  describe "traverse" $ do
    it "evaluates from left to right" $ property $
      \(tabs :: Tabs Int) -> execWriter (mapM (\x -> tell [x]) tabs) == map (\(Tab _ c _) -> c) (toList tabs)

    it "collects the results in order" $ property $
      \(tabs :: Tabs Int) -> runIdentity (traverse return tabs) == tabs

  describe "previous" $ do
    it "is inverse to next" $ property $
      \(tabs :: Tabs Int) -> (previous . next) tabs == tabs

    it "regards auto-close" $ property $
      \(tabs :: Tabs Int) tab -> previous (insert tab {tabCloseMode = AutoClose} tabs) == tabs

    it "keeps the tabs in order" $ property $
      \(tabs :: Tabs Int) -> toList (previous tabs) == toList tabs

  describe "next" $ do
    it "is inverse to previous" $ property $
      \(tabs :: Tabs Int) -> (next . previous) tabs == tabs

    it "regards auto-close" $ property $
      \(tabs :: Tabs Int) tab -> next (insert tab {tabCloseMode = AutoClose} tabs) == next tabs

    it "keeps the tabs in order" $ property $
      \(tabs :: Tabs Int) -> toList (next tabs) == toList tabs

  describe "insert" $ do
    it "regards auto-close" $ property $
      \tabs t1 t2 -> insert t2 (insert t1 {tabCloseMode = AutoClose} tabs) == insert t2 (tabs :: Tabs Int)

  describe "select" $ do

    it "keeps the tabs in order" $ property $
      \(tabs :: Tabs Int) name -> toList (select ((== name) . tabName) tabs) == toList tabs

{-
    prop "regards auto-close" $ \(tabs :: Tabs Int) tab -> do
      name <- tabName <$> elements (toList tabs)
      let p = (== name) . tabName
      return $ select p (insert tab {tabCloseMode = AutoClose} tabs) == select p tabs
      -}
