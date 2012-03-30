module InputSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Control.Monad.Identity
import           Key
import           Input hiding (readline)
import qualified Input

main :: IO ()
main = hspecX spec


type Input = InputT Identity


runInput :: Input a -> a
runInput = runIdentity . runInputT (error "runInput: end of input")

readline :: Input (Maybe String)
readline = Input.readline (const . return $ ())

spec :: Specs
spec = do

  describe "readline" $ do
    it "reads a line of user input" $ do
      runInput $ do
        unGetString "foo\n"
        readline
      `shouldBe` Just "foo"

    it "goes back in the history on ctrl-p" $ do
      runInput $ do
        unGetString $ "foo\n" ++ [ctrlP] ++ "\n"
        Just "foo" <- readline
        readline
      `shouldBe` Just "foo"
