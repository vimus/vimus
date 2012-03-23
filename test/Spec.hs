module Main (main) where

import           Test.Hspec.ShouldBe

import qualified CommandSpec
import qualified MacroSpec
import qualified TabSpec
import qualified ListWidgetSpec
import qualified KeySpec

main :: IO ()
main = hspecX $ do
  describe "MacroSpec" MacroSpec.spec
  describe "CommandSpec" CommandSpec.spec
  describe "TabSpec" TabSpec.spec
  describe "ListWidgetSpec" ListWidgetSpec.spec
  describe "KeySpec" KeySpec.spec
