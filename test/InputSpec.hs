module InputSpec (
  main
, spec

-- used by MacroSpec
, runInput
, userInput
) where

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Control.Applicative

import           Control.Monad.State
import           UI.Curses.Key
import           Key
import           Input hiding (readline)
import qualified Input

main :: IO ()
main = hspec spec

newtype AlphaNum1 = AlphaNum1 String
  deriving (Eq, Show)

instance Arbitrary AlphaNum1 where
  arbitrary = AlphaNum1 <$> (listOf1 . oneof) [choose ('0','9'), choose ('a','z'), choose ('A','Z')]

newtype AlphaNum = AlphaNum String
  deriving (Eq, Show)

instance Arbitrary AlphaNum where
  arbitrary = AlphaNum <$> (listOf1 . oneof) [choose ('0','9'), choose ('a','z'), choose ('A','Z')]

type Input = InputT (State String)

newtype SmallInt = SmallInt Int deriving (Eq, Show)

instance Arbitrary SmallInt where
  arbitrary = SmallInt . (`mod` 300) <$> arbitrary

-- | Provide test user input.
userInput :: String -> Input ()
userInput input = lift $ modify (++ input)

runInput :: Input a -> a
runInput action = (`evalState` "") . runInputT get_wch $ do
  r <- action
  input <- lift get
  unless (null input) (error $ "superfluous user input: " ++ input)
  return r
  where
    get_wch :: State String Char
    get_wch = do
      st <- get
      case st of
        x:xs -> put xs >> return x
        _    -> error "runInput: end of input"

readline :: Input String
readline = Input.readline noCompletion CommandHistory (const . return $ ())

infix 1 `shouldGive`

shouldGive :: String -> String -> Bool
input `shouldGive` expected
  | actual == expected = True
  | otherwise = error . unlines $ [
      "expected: " ++ expected
    , " but got: " ++ actual
    ]
  where
    actual = runInput (userInput input >> readline)

spec :: Spec
spec = do

  describe "return key" $ do
    it "accepts user input (cursor at end)" $
      "foo\n"
      `shouldGive`
      "foo"
    it "accepts user input" $
      "foobar" ++ replicate 3 keyLeft ++ "\n"
      `shouldGive`
      "foobar"
    it "does not add duplicates to the history" $ do
      runInput $ do
        userInput $ "foo\nbar\nbar\n" ++ [ctrlP,ctrlP] ++ "\n"
        "foo" <- readline
        "bar" <- readline
        "bar" <- readline
        readline
      `shouldBe` "foo"
    it "does not add empty lines to the history" $ do
      runInput $ do
        userInput $ "foo\n\n" ++ [ctrlP] ++ "\n"
        "foo" <- readline
        "" <- readline
        readline
      `shouldBe` "foo"
    it "does not add lines from the unGetBuffer to the history" $ do
      runInput $ do
        unGetString "foo\n"
        "foo" <- readline
        userInput $ [ctrlP] ++ "\n"
        readline
      `shouldBe` ""

  describe "ESC key" $ do
    it "cancels editing (cursor at end)" $
      "foo" ++ [keyEsc]
      `shouldGive`
      ""
    it "cancels editing" $ do
      "foobar" ++ replicate 3 keyLeft ++ [keyEsc]
      `shouldGive`
      ""

  -- * movement
  describe "left cursor key" $ do
    it "moves cursor to the left" $ do
      "foo" ++ [keyLeft] ++ "x\n"
      `shouldGive`
      "foxo"
  describe "right cursor key" $ do
    it "moves cursor to the right" $ do
      "foo" ++ [ctrlA, keyRight] ++ "x\n"
      `shouldGive`
      "fxoo"
  describe "ctrl-a" $ do
    it "places cursor at start" $ do
      "foo" ++ [ctrlA] ++ "x\n"
      `shouldGive`
      "xfoo"
  describe "ctrl-e" $ do
    it "places cursor at end" $ do
      "foo" ++ [ctrlA, ctrlE] ++ "x\n"
      `shouldGive`
      "foox"

  -- * editing
  describe "ctrl-d" $ do
    it "deletes the character to the right" $ do
      "fooxbar" ++ replicate 4 keyLeft ++ [keyDc] ++ "\n"
      `shouldGive`
      "foobar"

    it "does nothing if cursor is at end" $ property $
      \(AlphaNum1 xs) -> xs ++ [keyDc] ++ "\n" `shouldGive` xs

  describe "backspace key" $ do
    it "deletes the character to the left (cursor at end)" $ do
      "foox" ++ [keyBackspace] ++ "bar\n"
      `shouldGive`
      "foobar"
    it "deletes the character to the left" $ do
      "fooxbar" ++ replicate 3 keyLeft ++ [keyBackspace] ++ "\n"
      `shouldGive`
      "foobar"
    it "cancels editing if buffer is empty" $ do
      [keyBackspace]
      `shouldGive`
      ""
    it "does nothing if cursor is at start and buffer is non-empty" $ property $
      \(AlphaNum1 xs) -> xs ++ [ctrlA,keyBackspace] ++ "\n" `shouldGive` xs

  -- * history
  describe "ctrl-p" $ do
    it "goes back in history" $ do
      runInput $ do
        userInput $ "foo\n" ++ [ctrlP] ++ "\n"
        "foo" <- readline
        readline
      `shouldBe` "foo"

    it "places cursor at end when going back in history" $ do
      runInput $ do
        userInput $ "foo\n" ++ [ctrlP] ++ "x\n"
        "foo" <- readline
        readline
      `shouldBe` "foox"

    it "goes back in history (multiple times)" $ do
      runInput $ do
        userInput $ "foo\nbar\nbaz\nqux\n" ++ [ctrlP,ctrlP,ctrlP] ++ "\n"
        "foo" <- readline
        "bar" <- readline
        "baz" <- readline
        "qux" <- readline
        readline
      `shouldBe` "bar"

    it "keeps the current line, if the history is empty" $ do
      runInput $ do
        userInput $ "foo" ++ [ctrlP] ++ "\n"
        readline
      `shouldBe` "foo"

    it "keeps the current line, if the bottom of the history stack has been reached" $ do
      runInput $ do
        userInput $ "foo\nbar\nbaz\n" ++ replicate 3 ctrlP ++ "bar23" ++ [ctrlP] ++ "\n"
        "foo" <- readline
        "bar" <- readline
        "baz" <- readline
        readline
      `shouldBe` "foobar23"

    it "keeps the cursor position, when keeping the current line" $ do
      "foobar" ++ replicate 3 keyLeft ++ [ctrlP] ++ "x\n"
      `shouldGive`
      "fooxbar"

  describe "ctrl-n" $ do
    it "goes forward in history" $ do
      runInput $ do
        userInput $ "foo\nbar\n" ++ [ctrlP,ctrlP,ctrlN] ++ "\n"
        "foo" <- readline
        "bar" <- readline
        readline
      `shouldBe` "bar"

  describe "tab" $ do
    let tab input complete tabs = runInput $ do
          userInput $ input ++ replicate tabs keyTab ++ "\n"
          Input.readline (const complete) CommandHistory (const . return $ ())

    it "triggers autocompletion" $
      tab "foo" (Right "bar") 1 `shouldBe` "bar"

    context "cycled autocompletion through alternatives" $ do
      prop "does not autocomplete on the first tab press" $ \xs ->
        tab "foo" (Left xs) 1 `shouldBe` "foo"

      prop "does cycled autocompletion on later tab presses" $ \(SmallInt n) ->
        let alts  = ["bar","baz"]
            input = "foo"
            comps = alts ++ [input]
        in tab input (Left alts) (n + 2) `shouldBe` (cycle comps !! n)

      prop "does not complete anything given empty list of alternatives" $ \(SmallInt n) ->
        tab "foo" (Left []) n `shouldBe` "foo"
