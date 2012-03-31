module InputSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.QuickCheck hiding (property)
import           Control.Applicative

import           Control.Monad.Identity
import           UI.Curses.Key
import           Key
import           Input hiding (readline)
import qualified Input

main :: IO ()
main = hspecX spec

newtype AlphaNum1 = AlphaNum1 String
  deriving (Eq, Show)

instance Arbitrary AlphaNum1 where
  arbitrary = AlphaNum1 <$> (listOf1 . oneof) [choose ('0','9'), choose ('a','z'), choose ('A','Z')]

type Input = InputT Identity


runInput :: Input a -> a
runInput = runIdentity . runInputT (error "runInput: end of input")

readline :: Input String
readline = Input.readline (const . return $ ())

spec :: Specs
spec = do

  describe "readline" $ do

    describe "<CR>" $ do
      it "accepts user input (cursor at end)" $ do
        runInput $ do
          unGetString "foo\n"
          readline
        `shouldBe` "foo"
      it "accepts user input" $ do
        runInput $ do
          unGetString $ "foobar" ++ replicate 3 keyLeft ++ "\n"
          readline
        `shouldBe` "foobar"

    describe "ESC" $ do
      it "cancels editing (cursor at end)" $ do
        runInput $ do
          unGetString $ "foo" ++ [keyEsc]
          readline
        `shouldBe` ""
      it "cancels editing" $ do
        runInput $ do
          unGetString $ "foobar" ++ replicate 3 keyLeft ++ [keyEsc]
          readline
        `shouldBe` ""

  describe "readline (history)" $ do
    it "goes back in the history on ctrl-p" $ do
      runInput $ do
        unGetString $ "foo\n" ++ [ctrlP] ++ "\n"
        "foo" <- readline
        readline
      `shouldBe` "foo"

    it "keeps the current line on ctrl-p, if the history is empty" $ do
      runInput $ do
        unGetString $ "foo" ++ [ctrlP] ++ "\n"
        readline
      `shouldBe` "foo"

  describe "readline (movement)" $ do
    it "moves cursor to the left on left cursor key" $ do
      runInput $ do
        unGetString $ "foo" ++ [keyLeft] ++ "x\n"
        readline
      `shouldBe` "foxo"

    it "places cursor at the beginning on ctrl-a" $ do
      runInput $ do
        unGetString $ "foo" ++ [ctrlA] ++ "x\n"
        readline
      `shouldBe` "xfoo"

    it "moves cursor to the right on right cursor key" $ do
      runInput $ do
        unGetString $ "foo" ++ [ctrlA, keyRight] ++ "x\n"
        readline
      `shouldBe` "fxoo"

    it "places cursor at the end on ctrl-e" $ do
      runInput $ do
        unGetString $ "foo" ++ [ctrlA, ctrlE] ++ "x\n"
        readline
      `shouldBe` "foox"

  describe "readline (editing)" $ do
    it "deletes the left character on backspace (cursor at end)" $ do
      runInput $ do
        unGetString $ "foox" ++ [keyBackspace] ++ "bar\n"
        readline
      `shouldBe` "foobar"

    it "deletes the left character on backspace" $ do
      runInput $ do
        unGetString $ "fooxbar" ++ replicate 3 keyLeft ++ [keyBackspace] ++ "\n"
        readline
      `shouldBe` "foobar"

    it "cancels editing on backspace if buffer is empty" $ do
      runInput $ do
        unGetString $ [keyBackspace]
        readline
      `shouldBe` ""

    it "does nothing on backspace if cursor is at start and buffer is non-empty" $ property $
      \(AlphaNum1 xs) -> runInput (unGetString (xs ++ [ctrlA,keyBackspace] ++ "\n") >> readline) == xs
