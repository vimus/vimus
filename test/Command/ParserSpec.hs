{-# OPTIONS_GHC -fno-warn-orphans #-}
module Command.ParserSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.QuickCheck hiding (property)

import           Control.Applicative
import           Data.Foldable (asum)
import           Prelude hiding (takeWhile)
import           Command.Parser
import           Data.Char

main :: IO ()
main = hspec spec

instance Arbitrary ParseError where
  arbitrary = oneof [ pure Empty, ParseError <$> arbitrary
                    , SuperfluousInput <$> arbitrary
                    , MissingArgument <$> arbitrary
                    , InvalidArgument <$> arbitrary <*> arbitrary
                    , SpecificArgumentError <$> arbitrary
                    ]

spec :: Spec
spec = do

  describe "parserFail" $ do
    it "any other error takes precedence over empty" $ do
      property $ \err input ->
           runParser (empty <|> parserFail err) input
        == (Left err :: Either ParseError (String, String))
        &&
           runParser (parserFail err <|> empty) input
        == (Left err :: Either ParseError (String, String))

  describe "satisfy" $ do
    it "succeeds if given predicate holds" $ do
      runParser (satisfy (== 'a')) "abc" `shouldBe` Right ('a', "bc")

    it "fails if given predicate does not hold" $ do
      runParser (satisfy (== 'a')) "foo" `shouldBe` Left (ParseError "satisfy: unexpected 'f'")

    it "fails on empty input" $ do
      runParser (satisfy (== 'a')) "" `shouldBe` Left (ParseError "satisfy: unexpected end of input")

  describe "char" $ do
    it "recognizes a given character" $ property $
      \x xs -> runParser (char x) (x:xs) == Right (x, xs)

  describe "string" $ do
    it "recognizes a given string" $ property $
      \xs ys -> runParser (string xs) (xs ++ ys) == Right (xs, ys)

  describe "takeWhile" $ do
    it "takes from input as long as a given predicate holds" $ do
      runParser (takeWhile isAlpha) "foobar23test" `shouldBe` Right ("foobar", "23test")

    it "accepts an empty string" $ do
      runParser (takeWhile isAlpha) "23test" `shouldBe` Right ("", "23test")

  describe "takeWhile1" $ do
    it "takes from input as long as a given predicate holds" $ do
      runParser (takeWhile1 isAlpha) "foobar23test" `shouldBe` Right ("foobar", "23test")

    it "dose not accept an empty string" $ do
      runParser (takeWhile1 isAlpha) "23test" `shouldBe` Left (ParseError "takeWhile1: unexpected '2'")

  describe "endOfInput" $ do
    it "succeeds at end of input" $ do
      runParser (takeWhile isAlpha <* endOfInput) "foobar" `shouldBe` Right ("foobar", "")

    it "fails on remaining input" $ do
      runParser (takeWhile isAlpha <* endOfInput) "foo bar" `shouldBe` Left (ParseError "endOfInput: remaining input \" bar\"")

  describe "takeInput" $ do
    it "returns all remaining input" $
      property $ \xs -> runParser (takeInput) xs == Right (xs, "")


  describe "Alternative instance" $ do

    describe "empty" $ do
      it "fails on arbitrary input" $ property $ do
        \xs -> runParser (empty :: Parser String) xs == Left Empty

    describe "<|>" $ do
      it "chooses between two parsers" $ do
        runParser (takeWhile1 isAlpha <|> takeWhile1 isDigit) "23test" `shouldBe` Right ("23", "test")
        runParser (takeWhile1 isAlpha <|> takeWhile1 isDigit) "test23" `shouldBe` Right ("test", "23")

    describe "asum" $ do
      it "chooses between parsers" $ property $ \(NonEmpty xs) -> do
        x <- elements (xs :: [Char])
        let p = asum (map char xs)
        return $ runParser p [x] == Right (x, "")

  describe "Monad instance" $ do

    describe ">>" $ do
      it "chains parsers, returning the result of the last parser" $
        runParser (takeWhile1 (== 'a') >> takeWhile1 (== 'b') >> takeWhile1 (== 'c')) "aaabbbcccfoobar" `shouldBe` Right ("ccc", "foobar")
