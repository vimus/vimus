module Command.CommandSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Command.Command
import           Key

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

  describe "argumentErrorMessage" $ do
    it "works for one unexpected argument" $ do
      argumentErrorMessage 2 ["foo", "bar", "baz"] `shouldBe` "unexpected argument: baz"

    it "works for multiple unexpected arguments" $ do
      argumentErrorMessage 2 ["foo", "bar", "baz", "qux"] `shouldBe` "unexpected arguments: baz qux"

    it "works for missing arguments" $ do
      argumentErrorMessage 2 ["foo"] `shouldBe` "two arguments required"

  describe "parseMappingCommand" $ do

    it "parses command ShowAllMappings" $ do
      parseMappingCommand "" `shouldBe` Right ShowAllMappings

    it "parses command ShowMapping" $ do
      parseMappingCommand "foo" `shouldBe` Right (ShowMapping "foo")

    it "parses command AddMapping" $ do
      parseMappingCommand "foo bar baz" `shouldBe` Right (AddMapping "foo" "bar baz")

    it "handles key references" $ do
      parseMappingCommand "<c-a>x bar<c-b>baz<cr>" `shouldBe` Right (AddMapping [ctrlA, 'x'] $ "bar" ++ [ctrlB] ++ "baz\n")

    prop "ensures that mapping names and argumets are never null" $ \s ->
      case parseMappingCommand s of
        Right (AddMapping m a) -> (not . null) m && (not . null) a
        Right (ShowMapping m)  -> (not . null) m
        _                      -> True
