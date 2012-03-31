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

readline :: Input (Maybe String)
readline = Input.readline (const . return $ ())

spec :: Specs
spec = do

  describe "readline" $ do
    it "reads a line of user input" $ do
      runInput $ do
        unGetString "foo\n"
        readline
      `shouldBe` Just "foo"

    it "goes back in the history on ctrl-p" $ do
      runInput $ do
        unGetString $ "foo\n" ++ [ctrlP] ++ "\n"
        Just "foo" <- readline
        readline
      `shouldBe` Just "foo"

    it "keeps the current line on ctrl-p, if the history is empty" $ do
      runInput $ do
        unGetString $ "foo" ++ [ctrlP] ++ "\n"
        readline
      `shouldBe` Just "foo"

  describe "readline (movement)" $ do
    it "moves cursor to the left on left cursor key" $ do
      runInput $ do
        unGetString $ "foo" ++ [keyLeft] ++ "x\n"
        readline
      `shouldBe` Just "foxo"

    it "places cursor at the beginning on ctrl-a" $ do
      runInput $ do
        unGetString $ "foo" ++ [ctrlA] ++ "x\n"
        readline
      `shouldBe` Just "xfoo"

    it "moves cursor to the right on right cursor key" $ do
      runInput $ do
        unGetString $ "foo" ++ [ctrlA, keyRight] ++ "x\n"
        readline
      `shouldBe` Just "fxoo"

    it "places cursor at the end on ctrl-e" $ do
      runInput $ do
        unGetString $ "foo" ++ [ctrlA, ctrlE] ++ "x\n"
        readline
      `shouldBe` Just "foox"

  describe "readline (editing)" $ do
    it "deletes the left character on backspace (cursor at end)" $ do
      runInput $ do
        unGetString $ "foox" ++ [keyBackspace] ++ "bar\n"
        readline
      `shouldBe` Just "foobar"

    it "deletes the left character on backspace" $ do
      runInput $ do
        unGetString $ "fooxbar" ++ replicate 3 keyLeft ++ [keyBackspace] ++ "\n"
        readline
      `shouldBe` Just "foobar"

    it "cancels editing on backspace if buffer is empty" $ do
      runInput $ do
        unGetString $ [keyBackspace]
        readline
      `shouldBe` Nothing

    it "does nothing if cursor is at start and buffer is non-empty" $
      property $ \(AlphaNum1 xs) -> runInput (unGetString (xs ++ [ctrlA,keyBackspace] ++ "\n") >> readline) == Just xs
