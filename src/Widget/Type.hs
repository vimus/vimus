{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Widget.Type where

import           Data.String
import           Data.Monoid
import           Data.Default
import           WindowLayout

data WindowSize = WindowSize {
  windowSizeY :: Int
, windowSizeX :: Int
} deriving (Eq, Show)

instance Default WindowSize where
  def = WindowSize 25 80

-- | A chunk of text, possibly colored.
data Chunk = Colored WindowColor String | Plain String

instance IsString Chunk where
  fromString = Plain

-- | A line of text.
newtype TextLine = TextLine {unTextLine :: [Chunk]}

toPlainText :: TextLine -> String
toPlainText = concatMap unChunk . unTextLine
  where
    unChunk (Plain s)     = s
    unChunk (Colored _ s) = s

instance Monoid TextLine where
  mempty          = TextLine []
  xs `mappend` ys = TextLine (unTextLine xs ++ unTextLine ys)

instance IsString TextLine where
  fromString = TextLine . return . fromString

class Renderable a where
  renderItem :: a -> TextLine

instance Renderable String where
  renderItem = fromString

instance Renderable TextLine where
  renderItem = id
