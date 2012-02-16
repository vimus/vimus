module Util (withMPDEx_, maybeRead, match, MatchResult(..), addPlaylistSong, strip) where

import           Prelude hiding (catch)
import           Control.Exception
import           Data.List
import           Data.Char as Char
import Data.Maybe
import Control.Monad (liftM)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)

import Network.MPD

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

withMPDEx_ :: Maybe String -> Maybe Port -> MPD a -> IO (Response a)
withMPDEx_ h p m = do
    port       <- fromMaybeM (read `liftM` getEnvDefault "MPD_PORT" "6600") p
    (host, pw) <- parseHost `liftM` fromMaybeM (getEnvDefault "MPD_HOST" "localhost") h
    withMPDEx host port pw m
    where
        getEnvDefault x dflt =
            catch (getEnv x) (\e -> if isDoesNotExistError e
                                    then return dflt else ioError e)
        parseHost s = case breakChar '@' s of
                          (host, "") -> (host, "")
                          (pw, host) -> (host, pw)

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM a x = fromMaybe a (fmap return x)

-- Break a string by character, removing the separator.
breakChar :: Char -> String -> (String, String)
breakChar c s = let (x, y) = break (== c) s in (x, drop 1 y)

-- | Remove leading and trailing whitespace
strip :: String -> String
strip = dropWhile Char.isSpace . reverse . dropWhile Char.isSpace . reverse

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
