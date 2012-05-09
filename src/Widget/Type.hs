{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Widget.Type where
import           Data.Default

data WindowSize = WindowSize {
  windowSizeY :: Int
, windowSizeX :: Int
} deriving (Eq, Show)

instance Default WindowSize where
  def = WindowSize 25 80

class Renderable a where
  renderItem :: a -> String

instance Renderable String where
  renderItem = id
