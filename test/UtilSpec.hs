module UtilSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.QuickCheck hiding (property)

import           Util
import           Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "commonPrefix" $ do
    it "gets longest common prefix of a list of strings" $
      property $
        \prefix (NonEmpty xs) -> prefix `isPrefixOf` commonPrefix (map (prefix ++) xs)

  describe "clamp" $ do
    let
    it "confines a number to an interval" $ property prop_clamp

  where
    prop_clamp lower upper n_
      | upper <= lower  = n == lower
      | otherwise       = lower <= n && n < upper
      where
        n = clamp lower upper n_
