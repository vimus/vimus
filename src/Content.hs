module Content where

import qualified Data.Map as Map
import qualified Network.MPD as MPD hiding (withMPD)
import           System.FilePath (takeFileName)
import           Text.Printf (printf)
import           ListWidget (Searchable, searchTags)
import qualified Song
import           Type


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

-- | Show instance for Content
instance Renderable Content where
  renderItem item = case item of
    Song  song -> printf "%s - %s - %02s - %s" (Song.artist song) (Song.album song) (Song.track song) (Song.title song)
    Dir   path -> "[" ++ takeFileName path ++ "]"
    PList list -> "(" ++ takeFileName list ++ ")"
    PListSong _ _ song -> renderItem (Song song)

instance Searchable Content where
  searchTags item = case item of
    Dir   path -> [takeFileName path]
    PList path -> [takeFileName path]
    Song  song -> (concat $ Map.elems $ MPD.sgTags song) ++ [takeFileName $ MPD.sgFilePath song]
    PListSong _ _ song -> searchTags (Song song)
