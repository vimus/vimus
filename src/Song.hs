module Song where

import qualified Data.Map as Map
import           Data.List (intercalate)

import qualified Network.MPD as MPD hiding (withMPD)

import           System.FilePath (takeFileName)

artist, album, title, track :: MPD.Song -> String
artist = lookupMetadata MPD.Artist
album  = lookupMetadata MPD.Album
track  = lookupMetadata MPD.Track
title  = lookupMetadata' (takeFileName . MPD.sgFilePath) MPD.Title

-- | Get comma-separated list of meta data
lookupMetadata' :: (MPD.Song -> String) -> MPD.Metadata -> MPD.Song -> String
lookupMetadata' def key song = case Map.findWithDefault [] key tags of
  [] -> def song
  xs -> intercalate ", " xs
  where
    tags = MPD.sgTags song

lookupMetadata :: MPD.Metadata -> MPD.Song -> String
lookupMetadata = lookupMetadata' $ const "(none)"
