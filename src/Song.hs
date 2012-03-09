module Song (title, artist, album, track) where

import qualified Data.Map as Map
import           Data.List (intercalate)

import qualified Network.MPD as MPD hiding (withMPD)

import qualified System.FilePath as FilePath

takeFileName :: MPD.Path -> FilePath
takeFileName = FilePath.takeFileName . MPD.toString

artist :: MPD.Song -> String
artist = lookupMetadata MPD.Artist

album :: MPD.Song -> String
album  = lookupMetadata MPD.Album

track :: MPD.Song -> String
track  = lookupMetadata MPD.Track

title :: MPD.Song -> String
title  = lookupMetadata' (takeFileName . MPD.sgFilePath) MPD.Title

-- | Get comma-separated list of meta data
lookupMetadata' :: (MPD.Song -> String) -> MPD.Metadata -> MPD.Song -> String
lookupMetadata' def key song = case Map.findWithDefault [] key tags of
  [] -> def song
  xs -> intercalate ", " (map MPD.toString xs)
  where
    tags = MPD.sgTags song

lookupMetadata :: MPD.Metadata -> MPD.Song -> String
lookupMetadata = lookupMetadata' $ const "(none)"
