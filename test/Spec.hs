module Main (main) where

import           Test.Hspec.ShouldBe

import qualified KeySpec
import qualified TabSpec
import qualified UtilSpec
import qualified InputSpec
import qualified MacroSpec
import qualified Command.CommandSpec
import qualified ListWidgetSpec

main :: IO ()
main = hspecX $ do
  describe "KeySpec"              KeySpec.spec
  describe "TabSpec"              TabSpec.spec
  describe "UtilSpec"             UtilSpec.spec
  describe "InputSpec"            InputSpec.spec
  describe "MacroSpec"            MacroSpec.spec
  describe "Command.CommandSpec"  Command.CommandSpec.spec
  describe "ListWidgetSpec"       ListWidgetSpec.spec
