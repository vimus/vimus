{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (main) where

import UI.Curses hiding (wgetch, ungetch, mvaddstr)
import Control.Exception (finally)

import qualified Network.MPD as MPD hiding (withMPD)
import Network.MPD (withMPD_, Seconds, MonadMPD)

import Control.Monad.State (liftIO, gets, get, put, forever, runStateT, MonadIO)

import Data.Foldable (forM_)
import Data.List hiding (filter)
import Data.Maybe
import Data.IORef
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import System.Environment (getEnv)

import Control.Concurrent

import Text.Printf (printf)

import Prelude hiding (getChar)

import qualified WindowLayout
import qualified Input
import Macro

import ListWidget (ListWidget)
import qualified ListWidget

import qualified PlaybackState

import Option (getOptions)
import Util (strip)

import Control.Monad.Loops (whileM_)

import Vimus hiding (event)
import Command (runCommand, search, filter', globalCommands, makeListWidget, makeContentListWidget)

import qualified Song
import Content

------------------------------------------------------------------------
-- playlist widget

createListWidget :: (Show a, ListWidget.Searchable a, MonadIO m) => Window -> [a] -> m (ListWidget a)
createListWidget window songs = liftIO $ do
  (viewSize, _) <- getmaxyx window
  return $ ListWidget.new songs viewSize


handlePlaylist :: Handler (ListWidget Content)
handlePlaylist ev l = case ev of
  EvPlaylistChanged songs -> do
    return $ Just $ ListWidget.update l $ map Song songs

  _ -> return Nothing


handleLibrary :: Handler (ListWidget Content)
handleLibrary ev l = case ev of
  EvLibraryChanged songs -> do
    return $ Just $ ListWidget.update l $ map toContent songs

  _ -> return Nothing

handleBrowser :: Handler (ListWidget Content)
handleBrowser ev l = case ev of
  -- FIXME: Can we construct a data structure from `songs` and use this for the
  -- browser instead of doing MPD.lsInfo on every :move-out?
  EvLibraryChanged songs_ -> do
    songs <- MPD.lsInfo ""
    return $ Just $ ListWidget.update l $ map toContent songs

  _ -> return Nothing

------------------------------------------------------------------------
-- The main event loop
--

-- | Read file "~/.vimusrc", if it exists.
readVimusRc :: IO [String]
readVimusRc = do
  home <- getEnv "HOME"
  let vimusrc = home </> ".vimusrc"
  f <- doesFileExist vimusrc
  if f then (map strip . lines) `fmap` readFile vimusrc else return []

mainLoop ::  Vimus () -> Window -> Chan Notify -> IO Window -> Vimus ()
mainLoop initialize window chan onResize = do

  initialize

  forever $ do
    c <- getChar
    case c of
      -- a command
      ':' -> do
        mInput <- Input.readline_ window ":" getChar
        forM_ mInput $ \input -> do
          runCommand input
          renderMainWindow

      -- search
      '/' -> do
        mInput <- Input.readline searchPreview window "/" getChar
        forM_ mInput $ \input -> do
          search input

        -- window has to be redrawn, even if input is Nothing, otherwise the
        -- preview will remain on the screen
        renderMainWindow

      -- filter
      'F' -> do
        widget <- withCurrentWidget return
        cache  <- liftIO $ newIORef []
        mInput <- Input.readline (filterPreview widget cache) window "filter: " getChar
        forM_ mInput $ \input -> do
          filter' input

        -- window has to be redrawn, even if input is Nothing, otherwise the
        -- preview will remain on the screen
        renderMainWindow

      -- macro expansion
      _   -> do
        macros <- gets programStateMacros
        expandMacro macros getChar Input.ungetstr [c]
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

    getChar = do
      handleNotifies chan
      c <- Input.wgetch window
      if c == '\0'
        then getChar
        else if (c == keyResize) then do
          state <- get
          liftIO $ delwin $ mainWindow state
          win <- liftIO onResize
          size <- liftIO $ getmaxyx win
          put state { mainWindow = win }

          withAllWidgets $ sendEvent (EvResize size)

          renderMainWindow
          getChar
        else return c


data Notify = NotifyEvent Event
            | NotifyAction (Vimus ())


handleNotifies :: Chan Notify -> Vimus ()
handleNotifies chan = whileM_ (liftIO $ fmap not $ isEmptyChan chan) $ do
  notify <- liftIO $ readChan chan
  case notify of
    NotifyEvent event -> (withAllWidgets $ sendEvent event) >> renderMainWindow
    NotifyAction action   -> action


------------------------------------------------------------------------
-- mpd status

updateStatus :: (MonadIO m) => Window -> Window -> Maybe MPD.Song -> MPD.Status -> m ()
updateStatus songWindow playWindow mSong status = do

  putString songWindow song
  putString playWindow playState
  where
    song = maybe "none" Song.title mSong

    playState = stateSymbol ++ " " ++ formatTime current ++ " / " ++ formatTime total ++ " " ++ tags
      where
        (current, total) = PlaybackState.elapsedTime status
        stateSymbol = case MPD.stState status of
          MPD.Playing -> "|>"
          MPD.Paused  -> "||"
          MPD.Stopped -> "[]"

        tags = case filter (($ status) . fst) tagList of
          []   -> ""
          x:xs -> "[" ++ snd x ++ concatMap ((", "++) . snd) xs ++ "]"

        tagList = [
            (MPD.stRepeat ,  "repeat")
          , (MPD.stRandom ,  "random")
          , (MPD.stSingle ,  "single")
          , (MPD.stConsume, "consume")
          ]

    formatTime :: Seconds -> String
    formatTime s = printf "%02d:%02d" minutes seconds
      where
        (minutes, seconds) = s `divMod` 60

    putString :: (MonadIO m) => Window -> String -> m ()
    putString window string = liftIO $ do
      mvwaddstr window 0 0 string
      wclrtoeol window
      wrefresh window
      return ()


------------------------------------------------------------------------
-- Tabs

notifyEvent :: MonadIO m => Chan Notify -> Event -> m ()
notifyEvent chan = liftIO . writeChan chan . NotifyEvent

notifyLibraryChanged :: (MonadIO m, MonadMPD m) => Chan Notify -> m ()
notifyLibraryChanged chan = MPD.listAllInfo "" >>= notifyEvent chan . EvLibraryChanged

------------------------------------------------------------------------
-- Program entry point

run :: Maybe String -> Maybe String -> IO ()
run host port = do

  (onResize, tw, mw, statusWindow, songStatusWindow, playStatusWindow, inputWindow) <- WindowLayout.create

  let initialize = do

        -- source ~/.vimusrc
        -- FIXME:
        --  * proper error detection/handling
        vimusrc <- liftIO readVimusRc
        forM_ vimusrc $ \line ->
          case line of
            []        -> return ()
            '#':_     -> return ()
            s         -> runCommand s

        liftIO $ do
          -- It is critical, that this is only done after sourcing .vimusrc,
          -- otherwise :color commands are not effective and the user will see an
          -- annoying flicker!
          mvwaddstr inputWindow 0 0 "type 'q' to exit, read 'src/Macro.hs' for help"
          wrefresh inputWindow
          wrefresh statusWindow
          wrefresh songStatusWindow
          wrefresh playStatusWindow

        -- place cursor on current song, if any
        {- FIXME: set this back to work
        st <- MPD.status
        case MPD.stSongPos st of
          Just n -> modifyCurrentSongList (\l -> ListWidget.setPosition l n)
          _      -> return ()
        -}

        setCurrentView Playlist
        renderMainWindow
        return ()

  -- watch for playback and playlist changes
  notifyChan <- newChan
  forkIO $ withMPD $ PlaybackState.onChange
    (notifyEvent notifyChan . EvPlaylistChanged)
    (\song -> writeChan notifyChan . NotifyAction . updateStatus songStatusWindow playStatusWindow song)

  -- watch for library updates
  forkIO $ withMPD $ do
    notifyLibraryChanged notifyChan
    forever (MPD.idle [MPD.DatabaseS] >> notifyLibraryChanged notifyChan)


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

  let create = createListWidget mw ([] :: [Content])
  [pl, lw, bw, sr] <- sequence [create, create, create, create]
  hs <- createListWidget mw $ sort globalCommands

  withMPD $ runStateT (mainLoop initialize inputWindow notifyChan onResize) ProgramState {
      tabView           = tabFromList [
          (Playlist    , makeContentListWidget handlePlaylist pl)
        , (Library     , makeContentListWidget handleLibrary  lw)
        , (Browser     , makeContentListWidget handleBrowser  bw)
        , (SearchResult, makeContentListWidget noHandler      sr)
        , (Help        , makeListWidget        noHandler      hs)
        ]
    , mainWindow      = mw
    , statusLine      = statusWindow
    , tabWindow         = tw
    , getLastSearchTerm = ""
    , programStateMacros = defaultMacros
    , libraryPath        = Nothing
    }
  return ()

  where
    withMPD :: (MonadIO m) => MPD.MPD a -> m a
    withMPD action = do
      result <- liftIO $ withMPD_ host port action
      case result of
          Left  e -> fail $ show e
          Right r -> return r

    noHandler :: Handler a
    noHandler _ _ = return Nothing


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
