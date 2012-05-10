{-# LANGUAGE OverloadedStrings #-}
module Command.CompletionSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Vimus (Vimus)
import           Command.Core
import           Command.Completion

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "complete" $ do
    let commands = [
            command "foo" "" (undefined :: Vimus ())
          , command "bar" "" (undefined :: Vimus ())
          , command "baz" "" (undefined :: Vimus ())
          ]
        complete = completeCommand commands

    it "completes a command" $ do
      complete "f" `shouldBe` Right "foo "

    it "partially completes a command, on multiple matches with a common prefix" $ do
      complete "b" `shouldBe` Right "ba"

    it "gives suggestions, on multiple matches with no common prefix" $ do
      complete "ba" `shouldBe` Left ["bar", "baz"]
