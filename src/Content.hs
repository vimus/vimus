{-# OPTIONS_GHC -fno-warn-orphans #-}
module Content (
  Content(..)
, toContent
, Searchable(..)
) where

import qualified Data.Map as Map
import qualified Network.MPD as MPD hiding (withMPD)
import           System.FilePath (takeFileName)
import           Text.Printf (printf)
import           ListWidget (Renderable(..))
import qualified Song

-- | Define a new Content type to replace MPD.LsResult
data Content =
    Dir MPD.Path
  | Song MPD.Song
  | PList MPD.Path
  | PListSong MPD.PlaylistName Int MPD.Song
  deriving Show

toContent :: MPD.LsResult -> Content
toContent r = case r of
  MPD.LsSong song      -> Song song
  MPD.LsPlaylist list  -> PList list
  MPD.LsDirectory path -> Dir path

instance Renderable MPD.Song where
  renderItem song = printf "%s - %s - %02s - %s" (Song.artist song) (Song.album song) (Song.track song) (Song.title song)

instance Renderable Content where
  renderItem item = case item of
    Song  song -> renderItem song
    Dir   path -> "[" ++ takeFileName path ++ "]"
    PList list -> "(" ++ takeFileName list ++ ")"
    PListSong _ _ song -> renderItem song

class Searchable a where
  searchTags :: a -> [String]

instance Searchable Content where
  searchTags item = case item of
    Dir   path -> [takeFileName path]
    PList path -> [takeFileName path]
    Song  song -> searchTags song
    PListSong _ _ song -> searchTags song

instance Searchable MPD.Song where
  searchTags song = (concat $ Map.elems $ MPD.sgTags song) ++ [takeFileName $ MPD.sgFilePath song]
