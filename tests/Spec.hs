module Spec (main) where

import           Test.Hspec.ShouldBe

import qualified CommandSpec
import qualified MacroSpec

main :: IO ()
main = hspecX $ do
  describe "MacroSpec" MacroSpec.spec
  describe "CommandSpec" CommandSpec.spec
