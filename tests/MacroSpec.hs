module MacroSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit hiding (State)

import           Control.Monad.Trans.State.Lazy
import           Macro

data MacroTestState = MacroTestState {
  macroStateInput  :: String
, macroStateResult :: String
}

type MacroTestM = State MacroTestState

shouldExpandTo :: String -> String -> Assertion
macro `shouldExpandTo` expected = do
  let MacroTestState _ r = execState (expandMacro macros nextChar ungetstr "") (MacroTestState macro "")
  r `shouldBe` expected
  where
    macros =
        addMacro "bcd" ":three-letter-macro\n"
      . addMacro "bccdd" ":five-letter-macro\n"
      $ defaultMacros

    nextChar :: MacroTestM Char
    nextChar = do
      st <- get
      case macroStateInput st of
        x : xs -> do
          put st {macroStateInput = xs}
          return x
        _ ->
          return '\0'

    ungetstr :: String -> MacroTestM ()
    ungetstr s = modify (\st -> st {macroStateResult = macroStateResult st ++ s})

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "expandMacro" $ do
    it "expands a single-letter macro" $ do
      "q" `shouldExpandTo` ":close\n"

    it "expands a two-letter macro" $ do
      "gg" `shouldExpandTo` ":move-first\n"

    it "expands a three-letter macro" $ do
      "bcd" `shouldExpandTo` ":three-letter-macro\n"

    it "expands a five-letter macro" $ do
      "bccdd" `shouldExpandTo` ":five-letter-macro\n"
