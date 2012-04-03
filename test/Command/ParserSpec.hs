module Command.ParserSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Control.Applicative
import           Prelude hiding (takeWhile)
import           Command.Parser
import           Data.Char

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "takeWhile" $ do
    it "takes from input as long as a given predicate holds" $ do
      runParser (takeWhile isAlpha) "foobar23test" `shouldBe` Right ("foobar", "23test")

    it "accepts an empty string" $ do
      runParser (takeWhile isAlpha) "23test" `shouldBe` Right ("", "23test")

  describe "takeWhile1" $ do
    it "takes from input as long as a given predicate holds" $ do
      runParser (takeWhile1 isAlpha) "foobar23test" `shouldBe` Right ("foobar", "23test")

    it "dose not accept an empty string" $ do
      runParser (takeWhile1 isAlpha) "23test" `shouldBe` Left "takeWhile1: unexpected '2'"

  describe "<|>" $ do
    it "chooses between two parsers" $ do
      runParser (takeWhile1 isAlpha <|> takeWhile1 isDigit) "23test" `shouldBe` Right ("23", "test")
      runParser (takeWhile1 isAlpha <|> takeWhile1 isDigit) "test23" `shouldBe` Right ("test", "23")

  describe ">>" $ do
    it "chains parsers, returning the result of the last parser" $
      runParser (takeWhile1 (== 'a') >> takeWhile1 (== 'b') >> takeWhile1 (== 'c')) "aaabbbcccfoobar" `shouldBe` Right ("ccc", "foobar")
