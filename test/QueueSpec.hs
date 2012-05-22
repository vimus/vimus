module QueueSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Queue

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "takeAllQueue" $ do
    it "returns all elements in order" $ do
      q <- newQueue 
      putQueue q 10 >> putQueue q 20 >> putQueue q 30
      takeAllQueue q `shouldReturn` [10, 20, 30]

    it "leaves the queue empty" $ do
      q <- newQueue 
      putQueue q 10 >> putQueue q 20 >> putQueue q 30
      _ <- takeAllQueue q
      takeAllQueue q `shouldReturn` []
