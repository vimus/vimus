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
      \l -> execWriter (mapM (\x -> tell [x]) (l :: Tabs Int) ) == map (\(Tab _ c) -> c) (toList l)

    prop "collects the results in order" $
      \l -> runIdentity (traverse return l) == (l :: Tabs Int)
