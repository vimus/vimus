{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (main) where

import           Prelude hiding (getChar)
import           UI.Curses hiding (err, wgetch, wget_wch, ungetch, mvaddstr)
import qualified UI.Curses as Curses
import           Control.Exception (finally)

import qualified Network.MPD as MPD hiding (withMPD)
import           Network.MPD (Seconds, MonadMPD)

import           Control.Monad.State.Strict (lift, liftIO, get, put, forever, MonadIO)
import           Data.Foldable (forM_)
import           Data.List hiding (filter)
import           Data.IORef
import           System.Directory (doesFileExist)
import           Control.Concurrent (forkIO)
import           Text.Printf (printf)

import qualified WindowLayout
import qualified Input
import           Macro
import qualified PlaybackState
import           Option (getOptions)
import           Util (expandHome)
import           Queue
import           Vimus hiding (event)
import           Command (runCommand, source, search, filter_, createListWidget, makeContentListWidget, makeSongListWidget, handlePlaylist, handleLibrary, handleBrowser)
import qualified Song
import qualified Tab

------------------------------------------------------------------------
-- The main event loop
--
mainLoop :: Window -> Queue Notify -> IO Window -> Vimus ()
mainLoop window queue onResize = Input.runInputT wget_wch . forever $ do
  c <- Input.getChar
  case c of
    -- a command
    ':' -> do
      mInput <- Input.getInputLine_ window ":"
      forM_ mInput $ \input -> lift $ do
        runCommand input
        renderMainWindow

    -- search
    '/' -> do
      mInput <- Input.getInputLine searchPreview window "/"
      forM_ mInput $ \input -> lift $ do
        search input

      -- window has to be redrawn, even if input is Nothing, otherwise the
      -- preview will remain on the screen
      lift renderMainWindow

    -- filter
    'F' -> do
      widget <- lift (withCurrentWidget return)
      cache  <- liftIO $ newIORef []
      mInput <- Input.getInputLine (filterPreview widget cache) window "filter: "
      forM_ mInput $ \input -> lift $ do
        filter_ input

      -- window has to be redrawn, even if input is Nothing, otherwise the
      -- preview will remain on the screen
      lift renderMainWindow

    -- macro expansion
    _   -> do
      macros <- lift $ getMacros
      expandMacro macros Input.getChar Input.unGetString [c]
  where
    searchPreview term =
      withCurrentWidget $ \widget ->
        renderToMainWindow $ searchItem widget Forward term

    filterPreview widget cache term = do
      w <- liftIO $ do
        modifyIORef cache updateCache
        -- cache now contains results for all `inits term', in reverse order
        r <- readIORef cache
        (return . snd . head) r
      renderToMainWindow w
      where
        updateCache old@((t, w):xs)
          | term == t         = old
          | isPrefixOf t term = (term, filterItem w term) : old
          | otherwise         = updateCache xs
        -- applying filterItem to widget even if term is "" is crucial,
        -- otherwise the position won't be set to 0
        updateCache []               = [(term, filterItem widget term)]

    -- |
    -- A wrapper for wget_wch, that keeps the event queue running and handles
    -- resize events.
    wget_wch = do
      handleNotifies queue
      c <- liftIO (Curses.wget_wch window)
      if c == '\0'
        then wget_wch
        else if (c == keyResize) then do
          state <- get
          liftIO $ delwin $ mainWindow state
          win <- liftIO onResize
          size <- liftIO $ getmaxyx win
          put state { mainWindow = win }

          sendEvent (EvResize size)

          renderMainWindow
          wget_wch
        else return c


data Notify = NotifyEvent Event
            | NotifyError String
            | NotifyAction (Vimus ())


handleNotifies :: Queue Notify -> Vimus ()
handleNotifies q = do
  xs <- liftIO (takeAllQueue q)
  forM_ xs $ \x -> case x of
    NotifyEvent   event -> sendEvent event >> renderMainWindow
    NotifyError     err -> error err
    NotifyAction action -> action


------------------------------------------------------------------------
-- mpd status

updateStatus :: (MonadIO m) => Window -> Window -> Maybe MPD.Song -> MPD.Status -> m ()
updateStatus songWindow playWindow mSong status = do

  putString songWindow song ""
  putString playWindow playState tags
  where
    song = maybe "none" Song.title mSong

    playState = stateSymbol ++ " " ++ formatTime current ++ " / " ++ formatTime total
      where
        (current, total) = PlaybackState.elapsedTime status
        stateSymbol = case MPD.stState status of
          MPD.Playing -> "|>"
          MPD.Paused  -> "||"
          MPD.Stopped -> "[]"

    tags = case filter (($ status) . fst) tagList of
      []   -> ""
      x:xs -> snd x ++ concatMap ((", "++) . snd) xs

    tagList = [
          (MPD.stRepeat             ,   "repeat")
        , (MPD.stRandom             ,   "random")
        , (MPD.stSingle             ,   "single")
        , (MPD.stConsume            ,  "consume")
        , ((/= 0) . MPD.stUpdatingDb, "updating")
        ]

    formatTime :: Seconds -> String
    formatTime s = printf "%02d:%02d" minutes seconds
      where
        (minutes, seconds) = s `divMod` 60

    putString :: (MonadIO m) => Window -> String -> String -> m ()
    putString window string endstring = liftIO $ do
      (_, sizeX) <- getmaxyx window
      mvwaddstr window 0 0 string
      wclrtoeol window
      mvwaddstr window 0 (sizeX - length endstring) endstring
      wrefresh window
      return ()


------------------------------------------------------------------------
-- Tabs

notifyEvent :: MonadIO m => Queue Notify -> Event -> m ()
notifyEvent q e = liftIO $ q `putQueue` NotifyEvent e

notifyLibraryChanged :: (MonadIO m, MonadMPD m) => Queue Notify -> m ()
notifyLibraryChanged q = MPD.listAllInfo "" >>= notifyEvent q . EvLibraryChanged

------------------------------------------------------------------------
-- Program entry point

run :: Maybe String -> Maybe String -> IO ()
run host port = do

  (onResize, tw, mw, songStatusWindow, playStatusWindow, inputWindow) <- WindowLayout.create

  let initialize = do

        -- source ~/.vimusrc
        vimusrc <- liftIO (expandHome "~/.vimusrc")
        exists  <- liftIO (doesFileExist vimusrc)
        if exists
          then
            source vimusrc
          else liftIO $ do
            -- only print this if .vimusrc does not exist, otherwise it would
            -- overwrite possible config errors
            mvwaddstr inputWindow 0 0 "type 'q' to exit, read 'src/Macro.hs' for help"
            return ()

        liftIO $ do
          -- It is critical, that this is only done after sourcing .vimusrc,
          -- otherwise :color commands are not effective and the user will see an
          -- annoying flicker!
          wrefresh inputWindow
          wrefresh songStatusWindow
          wrefresh playStatusWindow

        renderTabBar
        renderMainWindow

        -- load default mappings
        runCommand "runtime default-mappings"

        return ()

  queue <- newQueue
  let withMPD_notifyError = withMPD (putQueue queue . NotifyError)

  -- watch for playback and playlist changes
  forkIO $ withMPD_notifyError $ PlaybackState.onChange
    (notifyEvent queue . EvPlaylistChanged)
    (notifyEvent queue . EvCurrentSongChanged)
    (\song -> putQueue queue . NotifyAction . updateStatus songStatusWindow playStatusWindow song)

  -- watch for library updates
  forkIO $ withMPD_notifyError $ do
    notifyLibraryChanged queue
    forever (MPD.idle [MPD.DatabaseS] >> notifyLibraryChanged queue)


  -- We use a timeout of 10 ms, but be aware that the actual timeout may be
  -- different due to a combination of two facts:
  --
  -- (1) ncurses getch (and related functions) returns when a signal occurs
  -- (2) the threaded GHC runtime uses signals for bookkeeping
  --     (see +RTS -V option)
  --
  -- So the effective timeout is swayed by the runtime.
  --
  -- We may workaround this in the future, as suggest here:
  -- http://www.serpentine.com/blog/2010/09/04/dealing-with-fragile-c-libraries-e-g-mysql-from-haskell/
  wtimeout inputWindow 10

  keypad inputWindow True

  pl <- createListWidget mw []
  lw <- createListWidget mw []
  bw <- createListWidget mw []

  let tabs = Tab.fromList [
          Tab Playlist (makeSongListWidget    handlePlaylist pl) Persistent
        , Tab Library  (makeSongListWidget    handleLibrary  lw) Persistent
        , Tab Browser  (makeContentListWidget handleBrowser  bw) Persistent
        ]

  withMPD error $ runVimus tabs mw inputWindow tw (initialize >> mainLoop inputWindow queue onResize)
  where
    withMPD onError action = do
      result <- MPD.withMPD_ host port action
      case result of
        Left  e  -> onError (show e)
        Right () -> return ()


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
  use_default_colors
  start_color

  curs_set 0

  finally (run host port) endwin
