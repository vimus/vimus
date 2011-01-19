module Command (
  runCommand
, searchPredicate
, filterPredicate
, search
, helpScreen
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromJust)

import System.Exit (exitSuccess)
import Vimus
import qualified Network.MPD as MPD hiding (withMPD)
import qualified ListWidget
import Control.Monad.State

import Control.Monad.Error (catchError)

import UI.Curses hiding (wgetch, ungetch, mvaddstr)
import Network.MPD ((=?), Seconds)
import Data.List
import Data.Char

import TextWidget (TextWidget)
import qualified TextWidget


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
  , Command "toggle"            $ MPD.toggle
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
        case MPD.sgIndex song of
          -- song is already on the playlist
          (Just i) -> MPD.play (Just i)
          -- song is not yet on the playlist
          Nothing  -> do
            i <- MPD.addId (MPD.sgFilePath song) Nothing
            MPD.play (Just $ MPD.ID i)

    -- insert a song right after the current song
  , Command "insert" $
      withCurrentSong $ \song -> do
        st <- MPD.status
        case MPD.stSongPos st of
          Just (MPD.Pos n)  -> do
            -- there is a current song, add after
            _ <- MPD.addId (MPD.sgFilePath song) (Just $ n + 1)
            modifyCurrentSongList ListWidget.moveDown
          _                 -> do
            -- there is no current song, just add
            eval "add"

    -- Remove given song from playlist
  , Command "remove" $
      withCurrentSong $ \song -> do
        case MPD.sgIndex song of
          (Just i) -> do MPD.delete i
          Nothing  -> return ()

  , Command "add-album" $
      withCurrentSong $ \song -> do
        songs <- MPD.find (MPD.Album =? MPD.sgAlbum song)
        MPD.addMany "" $ map MPD.sgFilePath songs

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
  case filter (isPrefixOf c) (Map.keys commandMap) of
    []   -> printStatus $ "unknown command: " ++ c
    [c_] -> fromJust $ Map.lookup c_ commandMap
    _    -> printStatus $ "ambiguous command: " ++ c
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
  let newTime = current + delta
  if (newTime < 0)
    then do
      -- seek within previous song
      case MPD.stSongPos st of
        Just (MPD.Pos currentSongPos) -> do
          playlist <- playlistWidget `liftM` get
          let previousSong = ListWidget.selectAt playlist (fromInteger currentSongPos - 1)
          MPD.seek (MPD.sgIndex previousSong) (MPD.sgLength previousSong + newTime)
        _ -> return ()
    else if (newTime > total) then
      -- seek within next song
      MPD.seek (MPD.stNextSongID st) (newTime - total)
    else
      -- seek within current song
      MPD.seek (MPD.stSongID st) newTime




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
searchPredicate_ _ term song =
 or [ match $ MPD.sgArtist song
    , match $ MPD.sgAlbum song
    , match $ MPD.sgTitle song
    , match $ MPD.sgFilePath song
    , match $ MPD.sgGenre song
    , match $ MPD.sgName song
    , match $ MPD.sgComposer song
    , match $ MPD.sgPerformer song
    --sgAux :: [(String, String)]
    ]
  where
    match s = isInfixOf term_ $ map toLower s
    term_ = map toLower term

