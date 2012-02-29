{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (main) where

import           Prelude hiding (getChar)
import           UI.Curses hiding (err, wgetch, wget_wch, ungetch, mvaddstr)
import qualified UI.Curses as Curses
import           Control.Exception (finally)

import qualified Network.MPD as MPD hiding (withMPD)
import           Network.MPD (Seconds, MonadMPD)

import           Control.Monad.State (lift, liftIO, gets, get, put, forever, evalStateT, MonadIO)
import           Data.Foldable (forM_)
import           Data.List hiding (filter)
import           Data.IORef
import           System.FilePath ((</>))
import           System.Directory (doesFileExist)
import           System.Environment (getEnv)
import           Control.Concurrent (forkIO)
import           Text.Printf (printf)

import qualified WindowLayout
import qualified Input
import           Macro
import           ListWidget (ListWidget)
import qualified ListWidget
import qualified PlaybackState
import           Option (getOptions)
import           Util (strip)
import           Queue
import           Vimus hiding (event)
import           Command (runCommand, search, filter', globalCommands, makeListWidget, makeContentListWidget)
import qualified Song
import           Content
import           Type

------------------------------------------------------------------------
-- playlist widget

createListWidget :: (Renderable a, ListWidget.Searchable a, MonadIO m) => Window -> [a] -> m (ListWidget a)
createListWidget window songs = liftIO $ do
  (viewSize, _) <- getmaxyx window
  return $ ListWidget.new songs viewSize

handleList :: Handler (ListWidget a)
handleList ev l = case ev of
  EvMoveUp         -> return . Just $ ListWidget.moveUp l
  EvMoveDown       -> return . Just $ ListWidget.moveDown l
  EvMoveFirst      -> return . Just $ ListWidget.moveFirst l
  EvMoveLast       -> return . Just $ ListWidget.moveLast l
  EvScrollUp       -> return . Just $ ListWidget.scrollUp l
  EvScrollDown     -> return . Just $ ListWidget.scrollDown l
  EvScrollPageUp   -> return . Just $ ListWidget.scrollPageUp l
  EvScrollPageDown -> return . Just $ ListWidget.scrollPageDown l
  EvResize (y, _)  -> return . Just $ ListWidget.setViewSize l y
  _                -> return Nothing

handlePlaylist :: Handler (ListWidget Content)
handlePlaylist ev l = case ev of
  EvPlaylistChanged songs -> do
    return $ Just $ ListWidget.update l $ map Song songs

  EvCurrentSongChanged song -> do
    return $ Just $ l `ListWidget.setMarked` (song >>= MPD.sgIndex)

  EvRemove -> do
    forM_ (ListWidget.select l >>= unContent >>= MPD.sgId) MPD.deleteId
    return Nothing

  _ -> handleList ev l
  where
    unContent (Song s) = Just s
    unContent _        = Nothing -- this should never happen

handleLibrary :: Handler (ListWidget Content)
handleLibrary ev l = case ev of
  EvLibraryChanged songs -> do
    let p x = case x of Song _ -> True; _ -> False
    return $ Just $ ListWidget.update l $ filter p $ map toContent songs

  _ -> handleList ev l

handleBrowser :: Handler (ListWidget Content)
handleBrowser ev l = case ev of

  -- FIXME: Can we construct a data structure from `songs_` and use this for the
  -- browser instead of doing MPD.lsInfo on every :move-out?
  EvLibraryChanged _ {- songs_ -} -> do
    songs <- MPD.lsInfo ""
    return $ Just $ ListWidget.update l $ map toContent songs

  EvMoveIn -> withSelected l $ \item -> do
    case item of
      Dir path -> do
        new <- map toContent `fmap` MPD.lsInfo path
        return . Just $ ListWidget.newChild new l
      PList path -> do
        new <- (map (uncurry $ PListSong path) . zip [0..]) `fmap` MPD.listPlaylistInfo path
        return . Just $ ListWidget.newChild new l
      Song  _    -> return Nothing
      PListSong  _ _ _ -> return Nothing

  EvMoveOut -> do
    case ListWidget.getParent l of
      Just p  -> return $ Just p
      Nothing -> return $ Just l

  _ -> handleList ev l

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
        filter' input

      -- window has to be redrawn, even if input is Nothing, otherwise the
      -- preview will remain on the screen
      lift renderMainWindow

    -- macro expansion
    _   -> do
      macros <- lift $ gets programStateMacros
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

  putString songWindow song
  putString playWindow playState
  where
    song = maybe "none" Song.title mSong

    playState = stateSymbol ++ " " ++ formatTime current ++ " / " ++ formatTime total ++ " " ++ tags ++ updating
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

        updating = if MPD.stUpdatingDb status /= 0 then " (updating)" else ""

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

notifyEvent :: MonadIO m => Queue Notify -> Event -> m ()
notifyEvent q e = liftIO $ q `putQueue` NotifyEvent e

notifyLibraryChanged :: (MonadIO m, MonadMPD m) => Queue Notify -> m ()
notifyLibraryChanged q = MPD.listAllInfo "" >>= notifyEvent q . EvLibraryChanged

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

        setCurrentView Playlist
        renderMainWindow
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

  let create = createListWidget mw ([] :: [Content])
  [pl, lw, bw, sr] <- sequence [create, create, create, create]
  hs <- createListWidget mw $ sort globalCommands

  withMPD error $ evalStateT (initialize >> mainLoop inputWindow queue onResize) ProgramState {
      tabView           = tabFromList [
          (Playlist    , makeContentListWidget handlePlaylist pl)
        , (Library     , makeContentListWidget handleLibrary  lw)
        , (Browser     , makeContentListWidget handleBrowser  bw)
        , (SearchResult, makeContentListWidget noHandler      sr)
        , (Help        , makeListWidget        (const Nothing) handleList     hs)
        ]
    , mainWindow      = mw
    , statusLine      = statusWindow
    , tabWindow         = tw
    , getLastSearchTerm = ""
    , programStateMacros = defaultMacros
    , libraryPath        = Nothing
    }
  where
    withMPD onError action = do
      result <- MPD.withMPD_ host port action
      case result of
        Left  e  -> onError (show e)
        Right () -> return ()

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
