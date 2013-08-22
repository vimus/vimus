{-# OPTIONS_GHC -fno-warn-orphans #-}
module FormatSpec where

import Test.Hspec

import           Command.Parser (ParseError(ParseError))
import qualified Song
import           Song (FormatTree(..))


instance Show a => Show (FormatTree s m a) where
  showsPrec _ Empty     = showString "Empty"
  showsPrec n (Pure a)  = showParen (n >= 11) (showString "Pure " . showsPrec 11 a)
  showsPrec n (x :+: y) = showParen (n >= 10) (showsPrec 10 x . showString " :+: " . showsPrec 10 y)
  showsPrec n (Alt xs)  = showParen (n >= 11) (showString "Alt " . showsPrec 11 xs)
  showsPrec _ (Meta _)  = showString "<function>"

instance Eq a => Eq (FormatTree s m a) where
  Empty  == Empty        = True
  Pure a == Pure b       = a == b
  Alt xs == Alt ys       = xs == ys
  (a :+: b) == (c :+: d) = a == c && b == d
  Meta _ == Meta _       = True
  _      == _            = False


spec :: Spec
spec = do

  describe "formatting" $ do

    let meta n | n > four = Just "world" | otherwise = Nothing
        four, seven :: Int
        four = 4
        seven = 7
        def = "default"

    it "formats flat patterns" $ do

      Song.format def () Empty                           `shouldBe` Nothing
      Song.format def () (Pure "hello")                  `shouldBe` Just "hello"
      Song.format def () (Pure "hello" :+: Pure "world") `shouldBe` Just "helloworld"
      Song.format def () (Pure "hello" :+: Empty)        `shouldBe` Nothing

    it "replaces failed top level (only) patterns with default value" $ do

      Song.format def four (Meta meta)       `shouldBe` Just "default"
      Song.format def four (Alt [Meta meta]) `shouldBe` Nothing

    it "provides alternative patterns and chooss the first that succeeds" $ do

      Song.format def seven (Pure "hello" :+: Alt [Pure "world", Pure "!!!"]) `shouldBe` Just "helloworld"
      Song.format def seven (Pure "hello" :+: Alt [Meta meta, Pure "!!!"])    `shouldBe` Just "helloworld"
      Song.format def four  (Pure "hello" :+: Alt [Meta meta, Pure "!!!"])    `shouldBe` Just "hello!!!"
      Song.format def seven (Pure "hello" :+: Alt [])                         `shouldBe` Nothing

  describe "parsing" $ do

    context "meta" $ do

      it "parses valid meta patterns" $ do

        Song.meta "valid%" `shouldBe` Right ("valid", "")
        Song.meta "valid\\%too%" `shouldBe` Right ("valid%too", "")
        Song.meta "valid%rest" `shouldBe` Right ("valid", "rest")

      it "errors on unterminated meta patterns" $

        Song.meta "foo" `shouldBe` Left (ParseError "unterminated meta pattern: %foo")

    context "alternatives" $ do

      it "parses valid alternatives patterns" $ do

        Song.alternatives "foo|bar|baz)" `shouldBe` Right (["foo","bar","baz"], "")
        Song.alternatives "foo\\|bar|baz)" `shouldBe` Right (["foo\\|bar","baz"], "")
        Song.alternatives "foo|bar|baz)rest" `shouldBe` Right (["foo","bar","baz"], "rest")

      it "respects nested alternatives patterns" $

        Song.alternatives "(foo|(bar|quux))|baz)" `shouldBe` Right (["(foo|(bar|quux))","baz"], "")

      it "errors on unterminated alternatives patterns" $

        Song.alternatives "foo" `shouldBe` Left (ParseError "unterminated alternatives pattern: (foo")

    context "parser" $ do

      it "reports non-supported meta patterns" $ do

        Song.parse "%foo%" `shouldBe` Left (ParseError "non-supported meta pattern: %foo%")

      it "parses nested alternatives" $ do

        Song.parse "hello(foo|(bar|baz))world" `shouldBe` Right
          (Pure "hello" :+: Alt [Pure "foo", Alt [Pure "bar", Pure "baz"]] :+: Pure "world")
