module Song where

import qualified Data.Map as Map
import           Data.List (intercalate)

import qualified Network.MPD as MPD hiding (withMPD)

artist, album, title, track :: MPD.Song -> String
artist = lookupMetadata MPD.Artist
album  = lookupMetadata MPD.Album
title  = lookupMetadata MPD.Title
track  = lookupMetadata MPD.Track

-- | Get comma-separated list of meta data
lookupMetadata :: MPD.Metadata -> MPD.Song -> String
lookupMetadata key song = case Map.findWithDefault [] key tags of
  [] -> "(none)"
  xs -> intercalate ", " xs
  where
    tags = MPD.sgTags song
