module Util where

import           Data.List (isPrefixOf)
import           Data.Char as Char
import           Data.Maybe (listToMaybe, fromJust)

import           Network.MPD (MonadMPD, PlaylistName, Id)
import qualified Network.MPD as MPD

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

-- | Add a song which is inside a playlist, returning its id.
addPlaylistSong :: MonadMPD m => PlaylistName -> Int -> m Id
addPlaylistSong plist index = do
  current <- MPD.playlistInfo Nothing
  MPD.load plist
  new <- MPD.playlistInfo Nothing

  let (first, this:rest) = splitAt index . map (fromJust . MPD.sgId) $ drop (length current) new
  mapM_ MPD.deleteId $ first ++ rest

  return this

-- | A copy of `System.Process.Internals.translate`.
posixEscape :: String -> String
posixEscape str = '\'' : foldr escape "'" str
  where escape '\'' = showString "'\\''"
        escape c    = showChar c
