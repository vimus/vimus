module Util where

import           Control.Applicative
import           Data.List (isPrefixOf)
import           Data.Char as Char
import           Data.Maybe (listToMaybe, fromJust)
import           System.FilePath ((</>))
import           System.Environment (getEnvironment)

import           Network.MPD (MonadMPD, PlaylistName, Id)
import qualified Network.MPD as MPD

-- | Remove leading and trailing whitespace
strip :: String -> String
strip = dropWhile Char.isSpace . reverse . dropWhile Char.isSpace . reverse

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads

data MatchResult = None | Match String | Ambiguous [String]
  deriving (Eq, Show)

match :: String -> [String] -> MatchResult
match s l = case filter (isPrefixOf s) l of
  []  -> None
  [x] -> Match x
  xs   -> if s `elem` xs then Match s else Ambiguous xs

-- | Get longest common prefix of a list of strings.
--
-- >>> commonPrefix ["foobar", "foobaz", "foosomething"]
-- "foo"
commonPrefix :: [String] -> String
commonPrefix [] = ""
commonPrefix xs = foldr1 go xs
  where
    go (y:ys) (z:zs)
      | y == z = y : go ys zs
    go _ _     = []

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


-- | Expand a tilde at the start of a string to the users home directory.
--
-- Expansion is only performed if the tilde is either followed by a slash or
-- the only character in the string.
expandHome :: FilePath -> IO (Either String FilePath)
expandHome path = do
  home <- maybe err Right . lookup "HOME" <$> getEnvironment
  case path of
    "~"            -> return home
    '~' : '/' : xs -> return $ (</> xs) `fmap` home
    xs             -> return (Right xs)
  where
    err = Left ("expansion of " ++ show path ++ " failed: $HOME is not defined")

-- | Confine a number to an interval.
--
-- The result will be greater or equal to a given lower bound and (if still
-- possible) smaller than a given upper bound.
clamp :: Int -- ^ lower bound (inclusive)
      -> Int -- ^ upper bound (exclusive)
      -> Int
      -> Int
clamp lower upper n = max lower $ min (pred upper) n
