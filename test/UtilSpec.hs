module UtilSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.QuickCheck hiding (property)

import           Util
import           Data.List

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "commonPrefix" $ do
    it "gets longest common prefix of a list of strings" $
      property $
        \prefix (NonEmpty xs) -> prefix `isPrefixOf` commonPrefix (map (prefix ++) xs)
