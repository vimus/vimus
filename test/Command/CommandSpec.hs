module Command.CommandSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Command.Core
import           Command.Parser
import           Command.Command

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
