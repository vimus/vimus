module Command (runCommand, printStatus, searchPredicate, search) where

import System.Exit (exitSuccess)
import Vimus
import qualified Network.MPD as MPD hiding (withMPD)
import qualified ListWidget
import Control.Monad.State
import UI.Curses hiding (wgetch, ungetch, mvaddstr)
import Network.MPD ((=?), Seconds)
import Data.List
import Data.Char

-- | Process a command
runCommand :: String -> Vimus ()
runCommand "exit"               = liftIO exitSuccess
runCommand "quit"               = liftIO exitSuccess
runCommand "next"               = MPD.next
runCommand "previous"           = MPD.previous
runCommand "toggle"             = MPD.toggle
runCommand "stop"               = MPD.stop
runCommand "update"             = MPD.update []
runCommand "clear"              = MPD.clear
runCommand "search-next"        = searchNext
runCommand "search-prev"        = searchPrev
runCommand "move-up"            = modifyCurrentSongList ListWidget.moveUp
runCommand "move-down"          = modifyCurrentSongList ListWidget.moveDown
runCommand "move-first"         = modifyCurrentSongList ListWidget.moveFirst
runCommand "move-last"          = modifyCurrentSongList ListWidget.moveLast
runCommand "scroll-up"          = modifyCurrentSongList ListWidget.scrollUp
runCommand "scroll-down"        = modifyCurrentSongList ListWidget.scrollDown
runCommand "scroll-page-up"     = modifyCurrentSongList ListWidget.scrollPageUp
runCommand "scroll-page-down"   = modifyCurrentSongList ListWidget.scrollPageDown
runCommand "window-library"     = setCurrentView Library
runCommand "window-playlist"    = setCurrentView Playlist

runCommand "seek-forward"       = seek 5
runCommand "seek-backward"      = seek (-5)
runCommand "window-next"        = modify (\s -> s { currentView = invert $ currentView s })
                                    where
                                      invert Playlist = Library
                                      invert Library  = Playlist
runCommand "play_"      = withCurrentSong play
                            where
                              play song = do
                                case MPD.sgIndex song of
                                  -- song is already on the playlist
                                  (Just i) -> MPD.play (Just i)
                                  -- song is not yet on the playlist
                                  Nothing  -> do
                                              i_ <- MPD.addId (MPD.sgFilePath song) Nothing
                                              let i = MPD.ID i_
                                              MPD.play (Just i)

-- insert a song right after the current song
runCommand "insert"     = withCurrentSong $ \song -> do
                            st <- MPD.status
                            case MPD.stSongPos st of
                              Just (MPD.Pos n)  -> do
                                -- there is a current song, add after
                                _ <- MPD.addId (MPD.sgFilePath song) (Just $ n + 1)
                                modifyCurrentSongList ListWidget.moveDown
                              _                 -> do
                                -- there is no current song, just add
                                runCommand "add"

runCommand "remove"     = withCurrentSong remove
                            where
                              -- | Remove given song from playlist
                              remove song = do
                                case MPD.sgIndex song of
                                  (Just i) -> do MPD.delete i
                                  Nothing  -> return ()

runCommand "add-album"  = withCurrentSong $ \song -> do
                            songs <- MPD.find (MPD.Album =? MPD.sgAlbum song)
                            MPD.addMany "" $ map MPD.sgFilePath songs

runCommand "add"        = withCurrentSong add
                            where
                              -- | Add given song to playlist
                              add song = do
                                _ <- MPD.addId (MPD.sgFilePath song) Nothing
                                modifyCurrentSongList ListWidget.moveDown
-- no command
runCommand c            = printStatus $ "unknown command: " ++ c



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
searchPredicate "" _ = False
searchPredicate term song =
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

