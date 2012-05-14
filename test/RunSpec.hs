module RunSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Run ()

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  -- this is currently only here, to make sure that everything is compiled with
  -- the test suite.
  return ()
