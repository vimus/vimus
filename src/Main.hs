{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Main where

import UI.Curses hiding (wgetch, ungetch, mvaddstr)
import Control.Exception (finally)
--import Control.Monad.Error.Class (throwError)
import System.Exit (exitSuccess)

import qualified Network.MPD as MPD hiding (withMPD)
import Network.MPD ((=?), Seconds)
import Network.MPD.Core

import Control.Monad.State
import Control.Monad.Error

import Data.Either (rights)
import Data.List
import Data.Char (toLower)
import Data.Maybe

import Control.Concurrent

import Text.Printf (printf)

import Prelude hiding (getChar)

import qualified WindowLayout
import qualified Input
import Macro (expandMacro)

import ListWidget (ListWidget)
import qualified ListWidget

import qualified PlaybackState
import PlaybackState (PlaybackState)

import Option (getOptions)
import Util (withMPDEx_)

import Control.Monad.Loops (whileM_)

------------------------------------------------------------------------
-- playlist widget

type SongListWidget = ListWidget MPD.Song

createSongListWidget :: (MonadIO m) => Window -> [MPD.Song] -> m SongListWidget
createSongListWidget window songs = liftIO $ do
  (sizeY, _) <- getmaxyx window
  return $ ListWidget.new renderOne songs sizeY
  where
    renderOne :: MPD.Song -> String
    renderOne song = MPD.sgArtist song ++ " - " ++ MPD.sgAlbum song ++ " - " ++ (show $ MPD.sgTrack song) ++ " - " ++  MPD.sgTitle song


updatePlaylist :: Vimus ()
updatePlaylist = do
  state <- get
  songs <- MPD.getPlaylist
  let newPlaylistWidget = ListWidget.update (playlistWidget state) songs
  put state { playlistWidget = newPlaylistWidget }


updateLibrary :: Vimus ()
updateLibrary = do
  state <- get
  -- it seems that we only get rights here, even with songs that have no
  -- id3tags attached
  songs <- fmap rights $ MPD.listAllInfo ""
  let newWidget = ListWidget.update (libraryWidget state) songs
  put state { libraryWidget = newWidget }


------------------------------------------------------------------------
-- commands

-- | Run given action with currently selected song
withCurrentSong :: (MPD.Song -> Vimus ()) -> Vimus ()
withCurrentSong action = do
  widget <- getCurrentWindow
  let song = ListWidget.select widget
  maybe (return ()) action song

-- | Process a command
runCommand :: String -> Vimus ()
runCommand "exit"               = liftIO exitSuccess
runCommand "quit"               = liftIO exitSuccess
runCommand "next"               = MPD.next
runCommand "previous"           = MPD.previous
runCommand "toggle"             = MPD.toggle
runCommand "stop"               = MPD.stop
runCommand "update"             = MPD.update []
runCommand "clear"              = MPD.clear >> updatePlaylist
runCommand "search-next"        = searchNext
runCommand "search-prev"        = searchPrev
runCommand "move-up"            = withCurrentWindow ListWidget.moveUp
runCommand "move-down"          = withCurrentWindow ListWidget.moveDown
runCommand "move-first"         = withCurrentWindow ListWidget.moveFirst
runCommand "move-last"          = withCurrentWindow ListWidget.moveLast
runCommand "scroll-up"          = withCurrentWindow ListWidget.scrollUp
runCommand "scroll-down"        = withCurrentWindow ListWidget.scrollDown
runCommand "scroll-page-up"     = withCurrentWindow ListWidget.scrollPageUp
runCommand "scroll-page-down"   = withCurrentWindow ListWidget.scrollPageDown
runCommand "window-library"     = modify (\s -> s { currentWindow = Library })
runCommand "window-playlist"    = modify (\s -> s { currentWindow = Playlist })

runCommand "window-next"        = modify (\s -> s { currentWindow = invert $ currentWindow s })
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
                                              updatePlaylist

runCommand "remove"     = withCurrentSong remove
                            where
                              -- | Remove given song from playlist
                              remove song = do
                                case MPD.sgIndex song of
                                  (Just i) -> do MPD.delete i
                                                 updatePlaylist
                                  Nothing  -> return ()

runCommand "add-album"  = withCurrentSong $ \song -> do
                            songs <- MPD.find (MPD.Album =? MPD.sgAlbum song)
                            MPD.addMany "" $ map MPD.sgFilePath songs

runCommand "add"        = withCurrentSong add
                            where
                              -- | Add given song to playlist
                              add song = do
                                _ <- MPD.addId (MPD.sgFilePath song) Nothing
                                updatePlaylist
                                withCurrentWindow ListWidget.moveDown
-- no command
runCommand c            = printStatus $ "unknown command: " ++ c


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
-- program state

data CurrentWindow = Playlist | Library


-- | Return currently selected song list.
getCurrentWindow :: (MonadState ProgramState m) => m SongListWidget
getCurrentWindow = do
  state <- get
  case currentWindow state of
    Playlist -> return $ playlistWidget state
    Library  -> return $ libraryWidget  state


-- | Modify currently selected song list by applying given function.
withCurrentWindow :: (MonadState ProgramState m) => (SongListWidget -> SongListWidget) -> m ()
withCurrentWindow func = modify $ \state ->
  case currentWindow state of
    Playlist -> state { playlistWidget = func $ playlistWidget state }
    Library  -> state { libraryWidget  = func $ libraryWidget  state }


data ProgramState = ProgramState {
  currentWindow   :: CurrentWindow
, playlistWidget  :: SongListWidget
, libraryWidget   :: SongListWidget
, mainWindow      :: Window
, statusLine      :: Window
, getLastSearchTerm :: String
}


instance MonadMPD (StateT ProgramState MPD) where
  open        = lift open
  close       = lift close
  send        = lift . send
  getPassword = lift getPassword

newtype Vimus a = Vimus {
  runVimus :: StateT ProgramState MPD a
} deriving (Monad, Functor, MonadIO, MonadState ProgramState, MonadError MPDError, MonadMPD)


renderMainWindow :: Vimus ()
renderMainWindow = getCurrentWindow >>= render

render :: ListWidget a -> Vimus ()
render l = do
  s <- get
  ListWidget.render (mainWindow s) l

------------------------------------------------------------------------
-- The main event loop

mainLoop :: Window -> Chan Notify -> IO Window -> Vimus ()
mainLoop window chan onResize = do

  -- We store the search term for search previews (search-as-you-type) in an
  -- MVar, this allows us to change it if the current search term changes (say
  -- the user types an other character) before processing of the preview action
  -- has been started.
  var <- liftIO $ newEmptyMVar
  let searchPreviewAction = searchPreview var

  forever $ do
    c <- getChar
    case c of
      ':' ->  do
                input <- Input.readline_ window ':' getChar
                maybe (return ()) (notify . NotifyCommand) input
      '/' ->  do
                input <- Input.readline searchPreviewAction window '/' getChar
                maybe (return ()) (notifyAction . search) input
      _   ->  do
                expandMacro getChar Input.ungetstr [c]
  where
    getChar = do
      handleNotifies chan
      c <- Input.wgetch window
      if c == '\0'
        then getChar
        else if (c == keyResize) then do
          state <- get
          liftIO $ delwin $ mainWindow state
          win <- liftIO onResize
          (sizeY, _) <- liftIO $ getmaxyx win

          let newPlaylistWidget = ListWidget.setViewSize (playlistWidget state) sizeY
          let newLibraryWidget  = ListWidget.setViewSize (libraryWidget state) sizeY

          put state {mainWindow = win, playlistWidget = newPlaylistWidget, libraryWidget = newLibraryWidget}
          renderMainWindow
          getChar
        else return c

    notify = liftIO . writeChan chan
    notifyAction = notify . NotifyAction

    searchPreview var term = do
      -- replace current search term with new one
      old <- liftIO $ tryTakeMVar var
      liftIO $ putMVar var term
      case old of
        Just _  ->
          -- there is still a notify up for processing, so we do not need to
          -- notify again
          return ()
        Nothing -> notifyAction $ do
          -- on each keystroke render a preview of the search, but do not
          -- modify any state
          t <- liftIO $ takeMVar var
          w <- getCurrentWindow
          render $ ListWidget.search (searchPredicate t) w


data Notify = NotifyPlaylistChanged
            | NotifyLibraryChanged
            | NotifyCommand String
            | NotifyAction (Vimus ())

handleNotifies :: Chan Notify -> Vimus ()
handleNotifies chan = whileM_ (liftIO $ fmap not $ isEmptyChan chan) $ do
  notify <- liftIO $ readChan chan
  case notify of
    NotifyPlaylistChanged -> updatePlaylist >> renderMainWindow
    NotifyLibraryChanged  -> updateLibrary >> renderMainWindow
    NotifyCommand c       -> runCommand c `catchError` (printStatus . show) >> renderMainWindow
    NotifyAction action   -> action


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
  withCurrentWindow $ searchMethod order $ searchPredicate term
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

------------------------------------------------------------------------
-- mpd status

updateStatus :: (MonadIO m) => Window -> Window -> PlaybackState -> m ()
updateStatus songWindow playWindow st = do

  putString songWindow song
  putString playWindow playState
  where
    song = fromMaybe "none" $ fmap MPD.sgTitle $ PlaybackState.currentSong st

    playState = stateSymbol ++ " " ++ formatTime current ++ " / " ++ formatTime total
      where
        (current, total) = PlaybackState.elapsedTime st
        stateSymbol = case PlaybackState.playState st of
          MPD.Playing -> "|>"
          MPD.Paused  -> "||"
          MPD.Stopped -> "[]"

    formatTime :: Seconds -> String
    formatTime s = printf "%02d:%02d" minutes seconds
      where
        minutes = s `div` 60
        seconds = s `mod` 60

    putString :: (MonadIO m) => Window -> String -> m ()
    putString window string = liftIO $ do
      mvwaddstr window 0 0 string
      wclrtoeol window
      wrefresh window
      return ()


------------------------------------------------------------------------
-- Program entry point

run :: Maybe String -> Maybe Port -> IO ()
run host port = do

  (onResize, mw, statusWindow, songStatusWindow, playStatusWindow, inputWindow) <- WindowLayout.create

  -- thread for playback state
  notifyChan <- newChan
  forkIO $ withMPD $ PlaybackState.onChange $ \st -> do
    writeChan notifyChan $ NotifyAction $ updateStatus songStatusWindow playStatusWindow st

  -- thread for asynchronous updates
  liftIO $ writeChan notifyChan NotifyPlaylistChanged
  liftIO $ writeChan notifyChan NotifyLibraryChanged
  forkIO $ withMPD $ forever $ do
    l <- MPD.idle
    when (MPD.Playlist `elem` l) $ do
      liftIO $ writeChan notifyChan NotifyPlaylistChanged
    when (MPD.Database `elem` l) $ do
      liftIO $ writeChan notifyChan NotifyLibraryChanged


  -- We use a timeout of 10 ms, but be aware that the actual timeout may be
  -- different due to a combination of two facts:
  --
  -- (1) ncurses getch (and related functions) returns when a signal occurs
  -- (2) the threaded GHC runtime uses signals for bookkeeping
  --     (see +RTS -V option)
  --
  -- So the effective timeout swayed by the runtime.
  --
  -- We may workaround this in the future, as suggest here:
  -- http://www.serpentine.com/blog/2010/09/04/dealing-with-fragile-c-libraries-e-g-mysql-from-haskell/
  wtimeout inputWindow 10

  keypad inputWindow True

  mvwaddstr inputWindow 0 0 "type 'q' to exit, read 'src/Macro.hs' for help"
  wrefresh inputWindow

  pl <- createPlaylistWidget mw
  lw <- createLibraryWidget mw

  withMPD $ runStateT (runVimus $ mainLoop inputWindow notifyChan onResize) ProgramState {
      currentWindow   = Playlist
    , playlistWidget  = pl
    , libraryWidget   = lw
    , mainWindow      = mw
    , statusLine      = statusWindow
    , getLastSearchTerm = ""
    }
  return ()

  where
    createPlaylistWidget :: Window -> IO SongListWidget
    createPlaylistWidget window = createSongListWidget window []

    createLibraryWidget :: Window -> IO SongListWidget
    createLibraryWidget window = createSongListWidget window []

    withMPD :: (MonadIO m) => MPD.MPD a -> m a
    withMPD action = do
      result <- liftIO $ withMPDEx_ host port action
      case result of
          Left  e -> fail $ show e
          Right r -> return r



main :: IO ()
main = do

  (host, port) <- getOptions

  -- recommended in ncurses manpage
  initscr
  raw
  noecho

  -- suggested  in ncurses manpage
  -- nonl
  intrflush stdscr True

  -- enable colors
  start_color
  use_default_colors

  curs_set 0

  finally (run host port) endwin
