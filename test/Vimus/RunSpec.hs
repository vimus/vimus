module Vimus.RunSpec (main, spec) where

import           Test.Hspec
import           Vimus.Run ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- this is currently only here, to make sure that everything is compiled with
  -- the test suite.
  return ()
