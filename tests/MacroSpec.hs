module Spec (main, spec) where

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
  let MacroTestState _ r = execState (expandMacro defaultMacros nextChar ungetstr "") (MacroTestState macro "")
  r `shouldBe` expected
  where
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
      "q" `shouldExpandTo` ":quit\n"

    it "expands a two-letter macro" $ do
      "gg" `shouldExpandTo` ":move-first\n"
