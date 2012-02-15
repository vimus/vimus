module Command (
  runCommand
, searchPredicate
, filterPredicate
, search
, helpScreen
) where

import           Data.List
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Char
import           Text.Printf (printf)
import           System.Exit (exitSuccess)
import           Control.Monad.State (get, modify, liftIO, liftM)
import           Control.Monad.Error (catchError)

import           Network.MPD ((=?), Seconds)
import qualified Network.MPD as MPD hiding (withMPD)
import qualified Network.MPD.Commands.Extensions as MPDE
import           UI.Curses hiding (wgetch, ungetch, mvaddstr)

import           Vimus
import           TextWidget (TextWidget)
import qualified TextWidget
import qualified ListWidget
import           Util (match, MatchResult(..), addPlaylistSong)

import           System.FilePath.Posix (takeFileName)

data Command = Command {
  name    :: String
, action  :: Vimus ()
}

command0 = Command

commands :: [Command]
commands = [
    command0 "help"               $ setCurrentView Help
  , command0 "exit"               $ liftIO exitSuccess
  , command0 "quit"               $ liftIO exitSuccess
  , command0 "next"               $ MPD.next
  , command0 "previous"           $ MPD.previous
  , command0 "toggle"             $ MPDE.toggle
  , command0 "stop"               $ MPD.stop
  , command0 "update"             $ MPD.update []
  , command0 "clear"              $ MPD.clear
  , command0 "search-next"        $ searchNext
  , command0 "search-prev"        $ searchPrev
  , command0 "move-up"            $ modifyCurrentSongList ListWidget.moveUp
  , command0 "move-down"          $ modifyCurrentSongList ListWidget.moveDown
  , command0 "move-first"         $ modifyCurrentSongList ListWidget.moveFirst
  , command0 "move-last"          $ modifyCurrentSongList ListWidget.moveLast
  , command0 "scroll-up"          $ modifyCurrentSongList ListWidget.scrollUp
  , command0 "scroll-down"        $ modifyCurrentSongList ListWidget.scrollDown
  , command0 "scroll-page-up"     $ modifyCurrentSongList ListWidget.scrollPageUp
  , command0 "scroll-page-down"   $ modifyCurrentSongList ListWidget.scrollPageDown
  , command0 "window-library"     $ setCurrentView Library
  , command0 "window-playlist"    $ setCurrentView Playlist
  , command0 "window-search"      $ setCurrentView SearchResult
  , command0 "window-browser"     $ setCurrentView Browser
  , command0 "seek-forward"       $ seek 5
  , command0 "seek-backward"      $ seek (-5)

  , command0 "window-next" $ do
      v <- getCurrentView
      case v of
        Playlist -> setCurrentView Library
        Library  -> setCurrentView Browser
        Browser  -> setCurrentView SearchResult
        SearchResult -> setCurrentView Playlist
        Help     -> setCurrentView Playlist

  , command0 "play_" $
      withCurrentSong $ \song -> do
        case MPD.sgId song of
          -- song is already on the playlist
          (Just i) -> MPD.playId i
          -- song is not yet on the playlist
          Nothing  -> MPD.addId (MPD.sgFilePath song) Nothing >>= MPD.playId

    -- insert a song right after the current song
  , command0 "insert" $
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
  , command0 "remove" $
      withCurrentSong $ \song -> do
        case MPD.sgId song of
          (Just i) -> do MPD.deleteId i
          Nothing  -> return ()

  , command0 "add-album" $
      withCurrentSong $ \song -> do
        case Map.lookup MPD.Album $ MPD.sgTags song of
          Just l -> do
            songs <- mapM MPD.find $ map (MPD.Album =?) l
            MPDE.addMany "" $ map MPD.sgFilePath $ concat songs
          Nothing -> printStatus "Song has no album metadata!"

    -- Add given song to playlist
  , command0 "add" $
      withCurrentSongList $ \list -> do
        case ListWidget.select list of
          Just (MPD.LsDirectory path) -> MPD.add_ path
          Just (MPD.LsPlaylist  plst) -> MPD.load plst
          Just (MPD.LsFile      song) ->
            case ListWidget.getParents list of
              p:_ -> case ListWidget.select p of
                Just (MPD.LsPlaylist pl) -> addPlaylistSong pl (ListWidget.getPosition list) >> return ()
                _                        -> addnormal
              _   -> addnormal
              where
                addnormal = MPD.add_ $ MPD.sgFilePath song
          Nothing -> return ()
        modifyCurrentSongList ListWidget.moveDown

  -- Browse inwards/outwards
  , command0 "move-in" $
      withCurrentItem $ \item -> do
        case item of
          MPD.LsDirectory path -> MPD.lsInfo path >>= modifyCurrentSongList . ListWidget.newChild
          MPD.LsPlaylist  list -> MPD.listPlaylistInfo list >>= modifyCurrentSongList . ListWidget.newChild . map MPD.LsFile
          _   -> return ()

  , command0 "move-out" $
      modifyCurrentSongList $ \list -> do
        case ListWidget.getParents list of
          p:_ -> p
          _   -> list
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
          let previousItem = ListWidget.selectAt playlist (currentSongPos - 1)
          case previousItem of
            MPD.LsFile song -> maybeSeek (MPD.sgId song) (MPD.sgLength song + newTime)
            _               -> return ()
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

searchPredicate :: String -> MPD.LsResult -> Bool
searchPredicate = searchPredicate_ False

filterPredicate :: String -> MPD.LsResult -> Bool
filterPredicate = searchPredicate_ True

searchPredicate_ :: Bool -> String -> MPD.LsResult -> Bool
searchPredicate_ onEmptyTerm "" _ = onEmptyTerm
searchPredicate_ _ term item = and $ map (\term_ -> or $ map (isInfixOf term_) tags) terms
  where
    tags :: [String]
    tags = map (map toLower) $ case item of
      MPD.LsDirectory path -> [takeFileName path]
      MPD.LsPlaylist  path -> [takeFileName path]
      MPD.LsFile      song -> concat $ Map.elems $ MPD.sgTags song
    terms = words $ map toLower term
