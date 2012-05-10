module CommandSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit.ShouldBe.Contrib

import           Command.Core
import           Command.Parser
import           Command

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "parseCommand" $ do
    it "parses a command" $ do
      parseCommand "add" `shouldBe` ("add", "")

    it "parses a command with arguments" $ do
      parseCommand "add some arguments" `shouldBe` ("add", "some arguments")

    it "ignores leading whitespace" $ do
      parseCommand "     add" `shouldBe` ("add", "")

    it "parses an exclamation mark as command" $ do
      parseCommand "!" `shouldBe` ("!", "")

    it "parses an exclamation mark with arguments as command" $ do
      parseCommand "!foo bar baz" `shouldBe` ("!", "foo bar baz")

    it "ignores whitespace before and after an exclamation mark" $ do
      parseCommand "    !  \t   foo bar baz" `shouldBe` ("!", "foo bar baz")

  describe "argument MacroExpansion" $ do
    it "is never null" $ property $
      \xs -> case runParser argumentParser xs of
        Left _ -> True
        Right (MacroExpansion ys, _) -> (not . null) ys

  describe "argument ShellCommand" $ do
    it "is never null" $ property $
      \xs -> case runParser argumentParser xs of
        Left _ -> True
        Right (ShellCommand ys, _) -> (not . null) ys

  describe "argument Volume" $ do
    it "returns exact volume value for positve integers" $ do
      runParser argumentParser "10" `shouldBe` (Right (Volume 10, ""))

    it "returns a positive offset if prefixed by +" $ do
      runParser argumentParser "+10" `shouldBe` (Right (VolumeOffset 10, ""))

    it "returns a negative offset if prefixed by -" $ do
      runParser argumentParser "-10" `shouldBe` (Right (VolumeOffset (-10), ""))

    it "returns nothing if given only a sign" $ do
      runParser (argumentParser :: Parser Volume) "+" `shouldSatisfy` isLeft

    it "fails if exact volume exceeds 0-100" $ do
      runParser (argumentParser :: Parser Volume) "110" `shouldSatisfy` isLeft

    it "fails if offset exceeds 0-100" $ do
      runParser (argumentParser :: Parser Volume) "+110" `shouldSatisfy` isLeft
