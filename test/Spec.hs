module Main (main) where

import           Test.Hspec.ShouldBe

import qualified KeySpec
import qualified TabSpec
import qualified UtilSpec
import qualified InputSpec
import qualified MacroSpec
import qualified ListWidgetSpec
import qualified Command.CoreSpec
import qualified Command.ParserSpec
import qualified Command.CommandSpec

main :: IO ()
main = hspecX $ do
  describe "KeySpec"              KeySpec.spec
  describe "TabSpec"              TabSpec.spec
  describe "UtilSpec"             UtilSpec.spec
  describe "InputSpec"            InputSpec.spec
  describe "MacroSpec"            MacroSpec.spec
  describe "ListWidgetSpec"       ListWidgetSpec.spec
  describe "Command.CoreSpec"     Command.CoreSpec.spec
  describe "Command.ParserSpec"   Command.ParserSpec.spec
  describe "Command.CommandSpec"  Command.CommandSpec.spec
