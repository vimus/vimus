module CommandSpec (main, spec) where

import           Test.Hspec.ShouldBe

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

  describe "argumentErrorMessage" $ do
    it "works for one unexpected argument" $ do
      argumentErrorMessage 2 ["foo", "bar", "baz"] `shouldBe` "unexpected argument: baz"

    it "works for multiple unexpected arguments" $ do
      argumentErrorMessage 2 ["foo", "bar", "baz", "qux"] `shouldBe` "unexpected arguments: baz qux"

    it "works for missing arguments" $ do
      argumentErrorMessage 2 ["foo"] `shouldBe` "two arguments required"

  describe "parseMapping" $ do

    it "parses an empty string" $ do
      parseMapping "" `shouldBe` ("", "")

    it "parses a mapping" $ do
      parseMapping "foo" `shouldBe` ("foo", "")

    it "parses a mapping with arguments" $ do
      parseMapping "foo bar baz" `shouldBe` ("foo", "bar baz")

    it "handles <cr> in mapping arguments" $ do
      parseMapping "foo bar<cr>baz" `shouldBe` ("foo", "bar\nbaz")

    it "never parses arguments without parsing a mapping name" $ property $
      \s -> case parseMapping s of (m, a) -> (null m && null a) || (not . null) m
