module MacroSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit hiding (State)

import           Macro
import           Data.Default

import           Input
import           InputSpec hiding (main, spec)

shouldExpandTo :: String -> String -> Assertion
macro `shouldExpandTo` expected = do
  let r = runInput (userInput macro >> expandMacro macros "" >> getUnGetBuffer)
  r `shouldBe` expected
  where
    macros =
        addMacro "bcd"   ":three-letter-macro\n"
      . addMacro "bccdd" ":five-letter-macro\n"
      . addMacro "q"     ":quit\n"
      . addMacro "gg"    ":move-first\n"
      $ def

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "expandMacro" $ do
    it "expands a single-letter macro" $ do
      "q" `shouldExpandTo` ":quit\n"

    it "expands a two-letter macro" $ do
      "gg" `shouldExpandTo` ":move-first\n"

    it "expands a three-letter macro" $ do
      "bcd" `shouldExpandTo` ":three-letter-macro\n"

    it "expands a five-letter macro" $ do
      "bccdd" `shouldExpandTo` ":five-letter-macro\n"
