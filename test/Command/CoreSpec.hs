{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Command.CoreSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Control.Applicative
import           UI.Curses (Color(..), magenta)
import           Command.Core
import           Command.Parser (runParser)

deriving instance Eq Color
deriving instance Show Color

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "readParser" $ do
    it "parses an integer" $ do
      runParser readParser "10" `shouldBe` Right (10 :: Int, "")

  describe "toAction" $ do
    it "works for arity 0" $ do
      toAction "foo" `runAction` "" `shouldBe` Right "foo"

    it "works for arity 1" $ do
      let f = id :: String -> String
      toAction f `runAction` "foo" `shouldBe` Right "foo"

    it "works for arity 2" $ do
      let f = (+) :: Int -> Int -> Int
      toAction f `runAction` "23 42" `shouldBe` Right (65 :: Int)

    it "works for arity 3" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      toAction f `runAction` "1.5 foo magenta" `shouldBe` Right (1.5 :: Double, "foo", magenta)

    it "ignores whitespace at the end of input" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      toAction f `runAction` "1.5 foo magenta   " `shouldBe` Right (1.5 :: Double, "foo", magenta)

    it "ignores whitespace in-between arguments" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      toAction f `runAction` "1.5   foo   magenta" `shouldBe` Right (1.5 :: Double, "foo", magenta)

    it "fails on missing argument" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      toAction f `runAction` "1.5 foo" `shouldBe` (Left "missing required argument: color" :: Either String (Double, String, Color))

    it "fails on invalid argument" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      toAction f `runAction` "1.5 foo foobar" `shouldBe` (Left "Argument \"foobar\" is not a valid color!" :: Either String (Double, String, Color))

    it "fails on unexpected argument" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      toAction f `runAction` "1.5 foo magenta foobar" `shouldBe` (Left "superfluous argument: \"foobar\"" :: Either String (Double, String, Color))

    it "<|> composes actions" $ do
      let action1 = (toAction $ \x -> show (x :: Int))   :: Action String
      let action2 = (toAction $ \x -> show (x :: Color)) :: Action String
      let action  = action1 <|> action2
      action `runAction` "magenta" `shouldBe` Right "Color 5"
      action `runAction` "23" `shouldBe` Right "23"

    it "<|> composes actions (regression test)" $ do
      let action1 = toAction (id  :: Int -> Int)        :: Action Int
      let action2 = toAction ((+) :: Int -> Int -> Int) :: Action Int
      (action1 <|> action2) `runAction` "23 42" `shouldBe` Right (65 :: Int)
      (action2 <|> action1) `runAction` "23 42" `shouldBe` Right (65 :: Int)

  describe "actionArguments" $ do
    it "given an action, it returns a list of required arguments" $ do
      let f x y z = (x, y, z) :: (Double, String, Color)
      actionArguments f (undefined :: (Double, String, Color)) `shouldBe` ["double", "string", "color"]
