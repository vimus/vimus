module KeySpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Key

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "expandKeys" $ do
    it "expands a single <cr>" $ do
      expandKeys "<cr>" `shouldBe` Right "\n"

    it "expands a single <c-a>" $ do
      expandKeys "<c-a>" `shouldBe` Right [ctrlA]

    it "works on a string with several key references" $ do
      expandKeys "foo <c-a><c-b> bar <cr> baz" `shouldBe` Right ("foo " ++ [ctrlA, ctrlB] ++ " bar " ++ "\n baz")

    it "respects backslash as an escape character for <" $ do
      expandKeys "\\<c-a>" `shouldBe` Right "<c-a>"

    it "replaces two backslashes with a single backslash" $ do
      expandKeys "foo\\\\bar" `shouldBe` Right "foo\\bar"

    it "fails on an unterminated key reference" $ do
      expandKeys "<foo" `shouldBe` Left (UnterminatedKeyReference "foo")

    it "fails on an empty key reference" $ do
      expandKeys "<>" `shouldBe` Left EmptyKeyReference

    it "fails on an unknown key reference" $ do
      expandKeys "<foo>" `shouldBe` Left (UnknownKeyReference "foo")

  describe "unExpandKeys" $ do
    it "replaces a single '\\n' with <CR>" $ do
      unExpandKeys "\n" `shouldBe` "<CR>"

    it "replaces ctrl-A with <C-A>" $ do
      unExpandKeys [ctrlA] `shouldBe` "<C-A>"

    it "works on a string with several special keys" $ do
      unExpandKeys ("foo " ++ [ctrlA, ctrlB] ++ " bar " ++ "\n baz") `shouldBe` "foo <C-A><C-B> bar <CR> baz"

    it "escapes an opening bracket" $ do
      unExpandKeys "<c-a>" `shouldBe` "\\<c-a>"

    it "escapes a backslashe" $ do
      unExpandKeys "foo\\bar" `shouldBe` "foo\\\\bar"

    it "is inverse to expandKeys" $ property $
      \s -> (expandKeys . unExpandKeys) s == Right s
