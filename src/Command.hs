module Command (
  runCommand
, searchPredicate
, filterPredicate
, search
, helpScreen
) where


import           Data.Map (Map, (!))
import qualified Data.Map as Map

import           Text.Printf (printf)

import System.Exit (exitSuccess)
import Vimus

import Network.MPD ((=?), Seconds)
import qualified Network.MPD as MPD hiding (withMPD)

import qualified Network.MPD.Commands.Extensions as MPDE

import qualified ListWidget
import Control.Monad.State (get, modify, liftIO, liftM)

import Control.Monad.Error (catchError)

import UI.Curses hiding (wgetch, ungetch, mvaddstr)
import Data.List
import Data.Char

import TextWidget (TextWidget)
import qualified TextWidget

import           Util (match, MatchResult(..))

data Command = Command {
  name    :: String
, action  :: Vimus ()
}


commands :: [Command]
commands = [
    Command "help"              $ setCurrentView Help
  , Command "exit"              $ liftIO exitSuccess
  , Command "quit"              $ liftIO exitSuccess
  , Command "next"              $ MPD.next
  , Command "previous"          $ MPD.previous
  , Command "toggle"            $ MPDE.toggle
  , Command "stop"              $ MPD.stop
  , Command "update"            $ MPD.update []
  , Command "clear"             $ MPD.clear
  , Command "search-next"       $ searchNext
  , Command "search-prev"       $ searchPrev
  , Command "move-up"           $ modifyCurrentSongList ListWidget.moveUp
  , Command "move-down"         $ modifyCurrentSongList ListWidget.moveDown
  , Command "move-first"        $ modifyCurrentSongList ListWidget.moveFirst
  , Command "move-last"         $ modifyCurrentSongList ListWidget.moveLast
  , Command "scroll-up"         $ modifyCurrentSongList ListWidget.scrollUp
  , Command "scroll-down"       $ modifyCurrentSongList ListWidget.scrollDown
  , Command "scroll-page-up"    $ modifyCurrentSongList ListWidget.scrollPageUp
  , Command "scroll-page-down"  $ modifyCurrentSongList ListWidget.scrollPageDown
  , Command "window-library"    $ setCurrentView Library
  , Command "window-playlist"   $ setCurrentView Playlist
  , Command "window-search"     $ setCurrentView SearchResult
  , Command "seek-forward"      $ seek 5
  , Command "seek-backward"     $ seek (-5)

  , Command  "window-next" $ do
      v <- getCurrentView
      case v of
        Playlist -> setCurrentView Library
        Library  -> setCurrentView SearchResult
        SearchResult -> setCurrentView Playlist
        Help     -> setCurrentView Playlist

  , Command "play_" $
      withCurrentSong $ \song -> do
        case MPD.sgId song of
          -- song is already on the playlist
          (Just i) -> MPD.playId i
          -- song is not yet on the playlist
          Nothing  -> MPD.addId (MPD.sgFilePath song) Nothing >>= MPD.playId

    -- insert a song right after the current song
  , Command "insert" $
      withCurrentSong $ \song -> do
        st <- MPD.status
        case MPD.stSongPos st of
          Just n -> do
            -- there is a current song, add after
            _ <- MPD.addId (MPD.sgFilePath song) (Just . fromIntegral $ n + 1)
            modifyCurrentSongList ListWidget.moveDown
          _                 -> do
            -- there is no current song, just add
            eval "add"

    -- Remove given song from playlist
  , Command "remove" $
      withCurrentSong $ \song -> do
        case MPD.sgId song of
          (Just i) -> do MPD.deleteId i
          Nothing  -> return ()

  , Command "add-album" $
      withCurrentSong $ \song -> do
        case Map.lookup MPD.Album $ MPD.sgTags song of
          Just l -> do
            songs <- mapM MPD.find $ map (MPD.Album =?) l
            MPDE.addMany "" $ map MPD.sgFilePath $ concat songs
          Nothing -> printStatus "Song has no album metadata!"

    -- Add given song to playlist
  , Command "add" $
      withCurrentSong $ \song -> do
        _ <- MPD.addId (MPD.sgFilePath song) Nothing
        modifyCurrentSongList ListWidget.moveDown
  ]


-- | Evaluate command with given name
eval :: String -> Vimus ()
eval "" = return ()
eval c = do
  case match c $ Map.keys commandMap of
    None         -> printStatus $ printf "unknown command %s" c
    Match x      -> commandMap ! x
    Ambiguous xs -> printStatus $ printf "ambiguous command %s, could refer to: %s" c $ intercalate ", " xs
  where

-- | Run command with given name
runCommand :: String -> Vimus ()
runCommand c = eval c `catchError` (printStatus . show) >> renderMainWindow

commandMap :: Map String (Vimus ())
commandMap = Map.fromList $ zip (map name commands) (map action commands)


helpScreen :: TextWidget
helpScreen = TextWidget.new $ map name commands


------------------------------------------------------------------------
-- commands

seek :: Seconds -> Vimus ()
seek delta = do
  st <- MPD.status
  let (current, total) = MPD.stTime st
  let newTime = round current + delta
  if (newTime < 0)
    then do
      -- seek within previous song
      case MPD.stSongPos st of
        Just currentSongPos -> do
          playlist <- playlistWidget `liftM` get
          let previousSong = ListWidget.selectAt playlist (currentSongPos - 1)
          maybeSeek (MPD.sgId previousSong) (MPD.sgLength previousSong + newTime)
        _ -> return ()
    else if (newTime > total) then
      -- seek within next song
      maybeSeek (MPD.stNextSongID st) (newTime - total)
    else
      -- seek within current song
      maybeSeek (MPD.stSongID st) newTime
  where
    maybeSeek (Just songId) time = MPD.seekId songId time
    maybeSeek Nothing _      = return ()




-- | Print a message to the status line
printStatus :: String -> Vimus ()
printStatus message = do
  status <- get
  let window = statusLine status
  liftIO $ mvwaddstr window 0 0 message
  liftIO $ wclrtoeol window
  liftIO $ wrefresh window
  return ()




------------------------------------------------------------------------
-- search

data SearchOrder = Forward | Backward

search :: String -> Vimus ()
search term = do
  modify $ \state -> state { getLastSearchTerm = term }
  search_ Forward term

searchNext :: Vimus ()
searchNext = do
  state <- get
  search_ Forward $ getLastSearchTerm state

searchPrev :: Vimus ()
searchPrev = do
  state <- get
  search_ Backward $ getLastSearchTerm state

search_ :: SearchOrder -> String -> Vimus ()
search_ order term = do
  modifyCurrentSongList $ searchMethod order $ searchPredicate term
  where
    searchMethod Forward  = ListWidget.search
    searchMethod Backward = ListWidget.searchBackward

searchPredicate :: String -> MPD.Song -> Bool
searchPredicate = searchPredicate_ False

filterPredicate :: String -> MPD.Song -> Bool
filterPredicate = searchPredicate_ True

searchPredicate_ :: Bool -> String -> MPD.Song -> Bool
searchPredicate_ onEmptyTerm "" _ = onEmptyTerm
searchPredicate_ _ term song = or $ map (isInfixOf term_) tags
  where
    tags :: [String]
    tags = map (map toLower) $ concat $ Map.elems $ MPD.sgTags song
    term_ = map toLower term

