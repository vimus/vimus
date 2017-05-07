-- | Songs metadata
module Vimus.Song (
  metaQueries
, album
, artist
, title
, track
, filename
) where

import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude hiding (length)
import qualified System.FilePath as FilePath
import           Text.Printf (printf)

import qualified Network.MPD as MPD hiding (withMPD)



type MetaQuery = MPD.Song -> Maybe String

metaQueries :: Map String MetaQuery
metaQueries = Map.fromList
  [ ("artist"    , artist)
  , ("album"     , album)
  , ("title"     , title)
  , ("track"     , track)
  , ("genre"     , genre)
  , ("year"      , year)
  , ("composer"  , composer)
  , ("performer" , performer)
  , ("comment"   , comment)
  , ("disc"      , disc)
  , ("length"    , length)
  , ("filename"  , filename)
  , ("directory" , directory)
  ]

-- | Song tags queries
artist, album, title, track, genre, year, composer, performer, comment, disc :: MetaQuery
[artist, album, title, genre, year, composer, performer, comment, disc] =
  map lookupMetadata
    [ MPD.Artist
    , MPD.Album
    , MPD.Title
    , MPD.Genre
    , MPD.Date
    , MPD.Composer
    , MPD.Performer
    , MPD.Comment
    , MPD.Disc
    ]

track = fmap (printf "%02s") . lookupMetadata MPD.Track

-- | Get comma-separated list of meta data
lookupMetadata :: MPD.Metadata -> MetaQuery
lookupMetadata key song = case Map.lookup key tags of
  Nothing -> Nothing
  Just xs -> Just $ intercalate ", " (map MPD.toString xs)
  where
    tags = MPD.sgTags song


-- | Song file queries
length, filename, directory :: MetaQuery
length s =
  let (minutes, seconds) = MPD.sgLength s `divMod` 60
  in Just $ printf "%d:%02d" minutes seconds

filename = Just . FilePath.takeFileName . MPD.toString . MPD.sgFilePath

directory = Just . FilePath.takeDirectory . MPD.toString . MPD.sgFilePath
