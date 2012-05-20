{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Content (
  Content(..)
, toContent
, Searchable(..)
) where

import qualified Data.Map as Map
import qualified Network.MPD as MPD hiding (withMPD)
import qualified System.FilePath as FilePath
import           Text.Printf (printf)

import           Widget.Type
import qualified Song

pathFileName :: MPD.Path -> String
pathFileName = FilePath.takeFileName . MPD.toString

plFileName :: MPD.PlaylistName -> String
plFileName = FilePath.takeFileName . MPD.toString

-- | Define a new Content type to replace MPD.LsResult
data Content =
    Dir MPD.Path
  | Song MPD.Song
  | PList MPD.PlaylistName
  | PListSong MPD.PlaylistName Int MPD.Song
  deriving (Show, Eq)

toContent :: MPD.LsResult -> Content
toContent r = case r of
  MPD.LsSong song      -> Song song
  MPD.LsPlaylist list  -> PList list
  MPD.LsDirectory path -> Dir path

instance Renderable MPD.Song where
  renderItem song = renderItem (printf "%s - %s - %02s - %s" (Song.artist song) (Song.album song) (Song.track song) (Song.title song) :: String)

instance Renderable Content where
  renderItem item = case item of
    Song  song         -> renderItem song
    Dir   path         -> renderItem $ "[" ++ pathFileName path ++ "]"
    PList list         -> renderItem $ "(" ++ plFileName list ++ ")"
    PListSong _ _ song -> renderItem song

class Searchable a where
  searchTags :: a -> [String]

instance Searchable String where
  searchTags = return

instance Searchable Content where
  searchTags item = case item of
    Dir   path -> [pathFileName path]
    PList path -> [plFileName path]
    Song  song -> searchTags song
    PListSong _ _ song -> searchTags song

instance Searchable MPD.Song where
  searchTags song = (pathFileName $ MPD.sgFilePath song) : (map MPD.toString $ concat $ Map.elems $ MPD.sgTags song)
