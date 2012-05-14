module CommandSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit.ShouldBe.Contrib

import           Command.Core
import           Command.Parser
import           Command

main :: IO ()
main = hspecX spec

type ParseResult a = Either ParseError (a, String)

spec :: Specs
spec = do
  describe "argument parser for MacroName" $ do
    it "parses key references" $ do
      runParser argumentParser "<ESC>" `shouldBe` Right (MacroName "\ESC", "")

    it "fails on unterminated key references" $ do
      runParser argumentParser "<ESC" `shouldBe` (Left . SpecificArgumentError $ "unterminated key reference \"ESC\"" :: ParseResult MacroName)

    it "fails on invalid key references" $ do
      runParser argumentParser "<foo>" `shouldBe` (Left . SpecificArgumentError $ "unknown key reference \"foo\"" :: ParseResult MacroName)

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
      runParser argumentParser "10" `shouldBe` Right (Volume 10, "")

    it "returns a positive offset if prefixed by +" $ do
      runParser argumentParser "+10" `shouldBe` Right (VolumeOffset 10, "")

    it "returns a negative offset if prefixed by -" $ do
      runParser argumentParser "-10" `shouldBe` Right (VolumeOffset (-10), "")

    it "returns nothing if given only a sign" $ do
      runParser (argumentParser :: Parser Volume) "+" `shouldSatisfy` isLeft

    it "fails if exact volume exceeds 0-100" $ do
      runParser (argumentParser :: Parser Volume) "110" `shouldSatisfy` isLeft

    it "fails if offset exceeds 0-100" $ do
      runParser (argumentParser :: Parser Volume) "+110" `shouldSatisfy` isLeft
