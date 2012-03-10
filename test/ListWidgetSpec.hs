{-# LANGUAGE StandaloneDeriving #-}
module ListWidgetSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           ListWidget

main :: IO ()
main = hspecX spec

deriving instance Eq Visible

spec :: Specs
spec = do

  describe "visible (an internal function)" $ do
    it "All" $ do
      visible 20 20 0 `shouldBe` All

    it "Top - Bot" $ do
      visible 20 19 0 `shouldBe` Top
      visible 20 19 1 `shouldBe` Bot

    it "Top - 50 - Bot" $ do
      visible 20 18 0 `shouldBe` Top
      visible 20 18 1 `shouldBe` Percent 50
      visible 20 18 2 `shouldBe` Bot

    it "Top - 33 - 66 - Bot" $ do
      visible 20 17 0 `shouldBe` Top
      visible 20 17 1 `shouldBe` Percent 33
      visible 20 17 2 `shouldBe` Percent 66
      visible 20 17 3 `shouldBe` Bot

    it "Top - 25 - 50 - 75 - Bot" $ do
      visible 20 16 0 `shouldBe` Top
      visible 20 16 1 `shouldBe` Percent 25
      visible 20 16 2 `shouldBe` Percent 50
      visible 20 16 3 `shouldBe` Percent 75
      visible 20 16 4 `shouldBe` Bot
