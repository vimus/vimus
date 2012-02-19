module Util where

import           Prelude hiding (catch)
import           Data.List
import           Data.Char as Char
import           Data.Maybe

import Network.MPD

-- | Remove leading and trailing whitespace
strip :: String -> String
strip = dropWhile Char.isSpace . reverse . dropWhile Char.isSpace . reverse

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

data MatchResult = None | Match String | Ambiguous [String]
  deriving (Eq, Show)

match :: String -> [String] -> MatchResult
match s l = case filter (isPrefixOf s) l of
  []  -> None
  [x] -> Match x
  xs   -> if s `elem` xs then Match s else Ambiguous xs

-- Add a song which is inside a playlist, returning its id

addPlaylistSong :: MonadMPD m => PlaylistName -> Int -> m Id
addPlaylistSong plist index = do
  current <- playlistInfo Nothing
  load plist
  new <- playlistInfo Nothing

  let (first, this:rest) = splitAt index . map (fromJust . sgId) $ drop (length current) new
  mapM_ deleteId $ first ++ rest

  return this

-- a copy of System.Process.Internals.translate
posixEscape :: String -> String
posixEscape str = '\'' : foldr escape "'" str
  where escape '\'' = showString "'\\''"
        escape c    = showChar c
