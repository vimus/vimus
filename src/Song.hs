module Song (title, artist, album, track, sortSongsBy) where

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Monoid (mconcat)
import           Data.List (intercalate,sortBy)
import           Data.Ord (comparing)

import qualified Network.MPD as MPD hiding (withMPD)

import qualified System.FilePath as FilePath
import Util (readMaybe)

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

-- | Get a meta-data with int type (defaults to 0)
lookupIntMetaData :: MPD.Metadata -> MPD.Song -> Int
lookupIntMetaData tag song = fromMaybe 0 $ Map.lookup tag (MPD.sgTags song) >>= listToMaybe >>= readMaybe . MPD.toString

-- | Sort the given song list using the given list of metadata in the specified order
sortSongsBy :: [MPD.Metadata] -> [MPD.Song] -> [MPD.Song]
sortSongsBy metas = sortBy (mconcat $ map cmp metas)
  where
    cmp MPD.Disc = comparing $ lookupIntMetaData MPD.Disc
    cmp MPD.Track = comparing $ lookupIntMetaData MPD.Track
    cmp x = comparing $ lookupMetadata' (const "") x
