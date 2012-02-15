module CommandSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Command

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "argumentErrorMessage" $ do
    it "works for one unexpected argument" $ do
      argumentErrorMessage 2 ["foo", "bar", "baz"] `shouldBe` "unexpected argument: baz"

    it "works for multiple unexpected arguments" $ do
      argumentErrorMessage 2 ["foo", "bar", "baz", "qux"] `shouldBe` "unexpected arguments: baz qux"

    it "works for missing arguments" $ do
      argumentErrorMessage 2 ["foo"] `shouldBe` "two arguments required"
