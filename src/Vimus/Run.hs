{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Vimus.Run (main) where

import           Prelude hiding (getChar)
import           UI.Curses hiding (err, wgetch, wget_wch, ungetch, mvaddstr)
import qualified UI.Curses as Curses
import           Control.Exception (finally)
import           Data.Maybe

import qualified Network.MPD as MPD hiding (withMPD)
import           Network.MPD (Seconds, MonadMPD)

import           Control.Monad.State.Strict (unless, lift, liftIO, forever, MonadIO)
import           Data.Foldable (forM_)
import           Data.List hiding (filter)
import           Data.IORef
import           System.Directory (doesFileExist)
import           Control.Concurrent (forkIO)
import           Text.Printf (printf)

import qualified Vimus.WindowLayout as WindowLayout
import qualified Vimus.Input as Input
import           Vimus.Input (HistoryNamespace(..), noCompletion)
import           Vimus.Macro
import qualified PlaybackState
import           Option (getOptions)
import           Vimus.Util (expandHome)
import           Vimus.Queue
import           Vimus.Type
import qualified Vimus.Command as Command
import qualified Vimus.Song as Song

------------------------------------------------------------------------
-- The main event loop
--
mainLoop :: Window -> Queue Notify -> IO Window -> Vimus ()
mainLoop window queue onResize = Input.runInputT wget_wch . forever $ do
  c <- Input.getChar
  case c of
    -- a command
    ':' -> do
      input <- Input.getInputLine_ window ":" CommandHistory Command.autoComplete
      unless (null input) $ lift $ do
        Command.runCommand input
        renderMainWindow

    -- search
    '/' -> do
      input <- Input.getInputLine searchPreview window "/" SearchHistory noCompletion
      unless (null input) $ lift $ do
        search input

      -- window has to be redrawn, even if input is Nothing, otherwise the
      -- preview will remain on the screen
      lift renderMainWindow

    -- filter
    'F' -> do
      widget <- lift getCurrentWidget
      cache  <- liftIO $ newIORef []
      input <- Input.getInputLine (filterPreview widget cache) window "filter: " SearchHistory noCompletion
      unless (null input) $ lift $ do
        filter_ input

      -- window has to be redrawn, even if input is Nothing, otherwise the
      -- preview will remain on the screen
      lift renderMainWindow

    -- macro expansion
    _   -> do
      macros <- lift getMacros
      expandMacro macros c
  where
    searchPreview term = do
      widget <- getCurrentWidget
      renderToMainWindow (searchItem widget Forward term)

    filterPreview :: AnyWidget -> IORef [(String, AnyWidget)] -> String -> Vimus ()
    filterPreview widget cache term = do
      w <- liftIO $ do
        modifyIORef cache updateCache
        -- cache now contains results for all `inits term', in reverse order
        r <- readIORef cache
        (return . snd . head) r
      renderToMainWindow w
      where
        updateCache :: [(String, AnyWidget)] -> [(String, AnyWidget)]
        updateCache old@((t, w):xs)
          | term == t           = old
          | t `isPrefixOf` term = (term, filterItem w term) : old
          | otherwise           = updateCache xs
        -- applying filterItem to widget even if term is "" is crucial,
        -- otherwise the position won't be set to 0
        updateCache []               = [(term, filterItem widget term)]

    -- |
    -- A wrapper for wget_wch, that keeps the event queue running and handles
    -- resize events.
    wget_wch = do
      handleNotifies queue
      c <- liftIO (Curses.wget_wch window)
      continue c
      where
        resize =  liftIO onResize >>= setMainWindow
        continue c
          | c == '\0'      = wget_wch
          | c == keyResize = resize >> wget_wch
          | otherwise      = return c


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
    song = fromMaybe "none" (Song.title =<< mSong)

    playState = stateSymbol ++ " " ++ formatTime current ++ " / " ++ formatTime total
                ++ volume
      where
        (current, total) = PlaybackState.elapsedTime status
        stateSymbol = case MPD.stState status of
          MPD.Playing -> "⏵"    -- U+23F5
          MPD.Paused  -> "⏸"    -- U+23F8
          MPD.Stopped -> "⏹"    -- U+23F9

        volume = maybe "" ((" vol: " ++) . show) (MPD.stVolume status)

    tags = intercalate ", " . map snd . filter (($ status) . fst) $ tagList

    tagList :: [(MPD.Status -> Bool, String)]
    tagList = [
          (isJust . MPD.stUpdatingDb, "updating")
        , (MPD.stRepeat             ,   "repeat")
        , (MPD.stRandom             ,   "random")
        , (MPD.stSingle             ,   "single")
        , (MPD.stConsume            ,  "consume")
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

run :: Maybe String -> Maybe String -> Bool -> IO ()
run host port ignoreVimusrc = do

  (onResize, tw, mw, songStatusWindow, playStatusWindow, inputWindow) <- WindowLayout.create

  let initialize = do

        -- load default mappings
        Command.runCommand "runtime default-mappings"

        -- source ~/.vimusrc
        r <- liftIO (expandHome "~/.vimusrc")
        flip (either printError) r $ \vimusrc -> do
          exists  <- liftIO (doesFileExist vimusrc)
          if not ignoreVimusrc && exists
            then
              Command.source vimusrc
            else liftIO $ do
              -- only print this if .vimusrc does not exist, otherwise it would
              -- overwrite possible config errors
              mvwaddstr inputWindow 0 0 "type :quit to exit, :help for help"
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

        return ()

  queue <- newQueue
  let withMPD_notifyError = withMPD (putQueue queue . NotifyError)

  -- watch for playback and playlist changes
  forkIO $ withMPD_notifyError $ PlaybackState.onChange
    (notifyEvent queue EvPlaylistChanged)
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

  withMPD error $ runVimus Command.tabs mw inputWindow tw (initialize >> mainLoop inputWindow queue onResize)
  where
    withMPD onError action = do
      result <- MPD.withMPD_ host port action
      case result of
        Left  e  -> onError (show e)
        Right () -> return ()


main :: IO ()
main = do

  (host, port, ignoreVimusrc) <- getOptions

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

  finally (run host port ignoreVimusrc) endwin
