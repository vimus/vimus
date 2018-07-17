module Vimus.RenderSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck hiding (UnicodeString)
import           Control.Applicative

import           Vimus.Render
import           Data.Char.WCWidth

newtype UnicodeString = UnicodeString String
  deriving Show

instance Arbitrary UnicodeString where
  arbitrary = UnicodeString <$> (listOf . elements) (['0'..'z'] ++ "\12456\12522\12483\12463\12539\12469\12486\12451\28092\23470\12495\12523\12498\12398\28040\22833\12472\12512\12494\12506\12487\12451\31532\30058")

main :: IO ()
main = hspec spec

wcstrlen :: String -> Int
wcstrlen = sum . map wcwidth

spec :: Spec
spec = do

  describe "fitToColumn" $ do
    it "determines number of characters that fit into a column of a given width" $
      property $ \(UnicodeString str) ->
        let n = fitToColumn str 20
            a = wcstrlen (take n str)
            b = wcstrlen (take (succ n) str)
        in a == b || (a <= 20 && 20 < b)
