{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Command.CoreSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Command.Core
import           UI.Curses (Color(..), magenta)

deriving instance Eq Color
deriving instance Show Color

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

  describe "readParser" $ do
    it "parses an integer" $ do
      readParser "10" `shouldBe` Right (10 :: Int, "")

  describe "toAction" $ do
    it "works for arity 0" $ do
      (unAction . toAction) "foo" "" `shouldBe` Right "foo"

    it "works for arity 1" $ do
      let f = id :: String -> String
      (unAction . toAction) f "foo" `shouldBe` Right "foo"

    it "works for arity 2" $ do
      let f = (+) :: Int -> Int -> Int
      (unAction . toAction) f "23 42" `shouldBe` Right (65 :: Int)

    it "works for arity 3" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      (unAction . toAction) f "1.5 foo magenta" `shouldBe` Right (1.5 :: Double, "foo", magenta)

    it "ignores whitespace at the end of input" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      (unAction . toAction) f "1.5 foo magenta   " `shouldBe` Right (1.5 :: Double, "foo", magenta)

    it "ignores whitespace at start of input" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      (unAction . toAction) f "   1.5 foo magenta" `shouldBe` Right (1.5 :: Double, "foo", magenta)

    it "ignores whitespace in-between arguments" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      (unAction . toAction) f "1.5   foo   magenta" `shouldBe` Right (1.5 :: Double, "foo", magenta)
