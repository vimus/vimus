module MacroSpec (main, spec) where

import           Prelude hiding (getChar)
import           Test.Hspec.ShouldBe
import           Test.HUnit hiding (State)

import           Macro
import           Data.Default

import           Input
import           InputSpec hiding (main, spec)

shouldExpandTo :: String -> String -> Assertion
macro `shouldExpandTo` expected = expand macro macros `shouldBe` expected


expand :: String -> Macros -> String
expand m ms = runInput (userInput m >> expandMacro_ ms >> getUnGetBuffer)

expandMacro_ :: Monad m => Macros -> InputT m ()
expandMacro_ ms = getChar >>= expandMacro ms

macros :: Macros
macros =
    addMacro "bcd"   ":three-letter-macro\n"
  . addMacro "bccdd" ":five-letter-macro\n"
  . addMacro "q"     ":quit\n"
  . addMacro "gg"    ":move-first\n"
  $ def

main :: IO ()
main = hspec spec

spec :: Spec
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

    it "discards partial matches" $ do
      let ms = addMacro "foo" ":foo"
             . addMacro "bar" ":bar"
             $ def
      expand "foo" ms `shouldBe` ":foo"
      expand "bar" ms `shouldBe` ":bar"
      r <- return . runInput $ do
        userInput "fobar"
        expandMacro_ ms  -- discards 'f'
        expandMacro_ ms  -- discards 'o'
        expandMacro_ ms
        getUnGetBuffer
      r `shouldBe` ":bar"

  describe "expandMacro (regression tests)" $ do
    it "ignores input that does not match any macro" $ do
      let ms = addMacro "ab" "foo" def
      expand "ab" ms `shouldBe` "foo"
      r <- return . runInput $ do
        userInput "bab"
        expandMacro_ ms
        expandMacro_ ms
        getUnGetBuffer
      r `shouldBe` "foo"

  describe "removeMacro" $ do
    it "removes a macro" $ do
      expand "q" macros `shouldBe` ":quit\n"
      let Right ms = removeMacro "q" macros
      expand "q" ms     `shouldBe` ""
