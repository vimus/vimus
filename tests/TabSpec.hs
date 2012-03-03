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
  arbitrary = Tab <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (TabZipper a) where
  arbitrary = TabZipper <$> arbitrary <*> arbitrary

deriving instance (Eq a) => Eq (Tab a)
deriving instance (Eq a) => Eq (TabZipper a)
deriving instance (Show a) => Show (Tab a)
deriving instance (Show a) => Show (TabZipper a)

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "traverse" $ do
    prop "evaluates from left to right" $
      \l -> execWriter (mapM (\x -> tell [x]) (l :: TabZipper Int) ) == map (\(Tab _ c) -> c) (getTabs l)

    prop "collects the results in order" $
      \l -> runIdentity (traverse return l) == (l :: TabZipper Int)
