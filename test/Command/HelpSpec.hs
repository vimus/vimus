{-# LANGUAGE QuasiQuotes #-}
module Command.HelpSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Data.String
import           Command.Help

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "help" $ do
    it "strips leading space" $ do
      unHelp [help|
      foo bar
      |] `shouldBe` ["foo bar"]

    it "combines subsequent non-empty lines" $ do
      unHelp [help|
      foo
      bar
      |] `shouldBe` ["foo bar"]

    it "treats an empty line as a paragraph separator" $ do
      unHelp [help|
      foo

      bar
      |] `shouldBe` ["foo", "bar"]

    it "treats several empty lines as a paragraph separator" $ do
      unHelp [help|
      foo



      bar
      |] `shouldBe` ["foo", "bar"]

    it "does automatic word wrapping" $ do
      unHelp [help|
      Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
      veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
      commodo consequat.  Duis aute irure dolor in reprehenderit in voluptate
      velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
      occaecat cupidatat non proident, sunt in culpa qui officia deserunt
      mollit anim id est laborum.
      |] `shouldBe` [
          "Lorem ipsum dolor sit amet, consectetur adipisicing elit,"
        , "sed do eiusmod tempor incididunt ut labore et dolore magna"
        , "aliqua. Ut enim ad minim veniam, quis nostrud exercitation"
        , "ullamco laboris nisi ut aliquip ex ea commodo consequat."
        , "Duis aute irure dolor in reprehenderit in voluptate velit"
        , "esse cillum dolore eu fugiat nulla pariatur. Excepteur sint"
        , "occaecat cupidatat non proident, sunt in culpa qui officia"
        , "deserunt mollit anim id est laborum."
        ]

    it "produces empty output on empty input" $ do
      unHelp [help||] `shouldBe` []

    it "produces empty output on all-whitespace input" $ do
      unHelp [help|
      
      |] `shouldBe` []

    it "puts a word that is longer than the text width on a separate line" $ do
      unHelp [help|
      http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html
      |] `shouldBe` ["http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html"]

    it "corretly handles the case, where the input is exactly one line and one word" $ do
      unHelp [help|
      Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
      |] `shouldBe` [
          "Lorem ipsum dolor sit amet, consectetur adipisicing elit,"
        , "sed"
        ]

    context "when given arbitrary input" $ do
      it "ensures that lines never exceed the text width (or only consist of a single word)" $ property $
        all (\x -> length x <= 60 || length (words x) == 1) . unHelp . fromString
