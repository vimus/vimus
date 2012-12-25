{-# LANGUAGE CPP, OverloadedStrings, QuasiQuotes, TupleSections, RecordWildCards #-}
module Command (
  runCommand
, autoComplete
, source
, tabs

#ifdef TEST
, MacroName (..)
, MacroExpansion (..)
, ShellCommand (..)
, Volume(..)
#endif
) where

import           Data.Function
import           Data.List
import           Data.Maybe (catMaybes)
import           Data.Char
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Ord (comparing)
import           Data.Monoid (mappend)
import           Control.Monad (void, when, unless, guard)
import           Control.Applicative
import           Data.Foldable (forM_, for_)
import           Data.Traversable (for)
import           Text.Printf (printf)
import           Text.Read (readMaybe)
import           System.Exit
import           System.Cmd (system)

import           System.Directory (doesFileExist)
import           System.FilePath ((</>), dropFileName)
import           Data.Map (Map, (!))
import qualified Data.Map as Map

import           Data.Time.Clock.POSIX

import           Control.Monad.State.Strict (gets, liftIO, MonadIO)
import           Control.Monad.Error (catchError)

import           Network.MPD ((=?))
import qualified Network.MPD as MPD hiding (withMPD)
import qualified Network.MPD.Commands.Extensions as MPDE
import qualified Network.MPD.Applicative as MPDA

import           UI.Curses hiding (wgetch, ungetch, mvaddstr, err)

import           Paths_vimus (getDataFileName)

import           Util
import           Vimus
import           Widget.Type (Renderable)
import           Widget.ListWidget (ListWidget)
import qualified Widget.ListWidget as ListWidget
import           Widget.HelpWidget
import           Content
import           WindowLayout
import           Key (ExpandKeyError (..), keyNames, expandKeys)
import qualified Macro
import           Input (CompletionFunction)
import           Command.Core
import           Command.Help (help)
import           Command.Parser
import           Command.Completion

import           Tab (Tabs)
import qualified Tab

-- | Initial tabs after startup.
tabs :: Tabs AnyWidget
tabs = Tab.fromList [
    tab Playlist (PlaylistWidget (const False) 0)
  , tab Library  LibraryWidget
  , tab Browser  BrowserWidget
  ]
  where
    tab :: Widget w => TabName -> (ListWidget a -> w) -> Tab AnyWidget
    tab n t = Tab n (AnyWidget . t $ ListWidget.new []) Persistent

data PlaylistWidget = PlaylistWidget {
  plMarked     :: MPD.Song -> Bool
, plLastAction :: POSIXTime
, plSongs      :: ListWidget MPD.Song
}

instance Widget PlaylistWidget where
  render         PlaylistWidget{..}     = ListWidget.render plMarked plSongs
  currentItem    PlaylistWidget{..}     = Song <$> ListWidget.select plSongs
  searchItem  pl@PlaylistWidget{..} o s = pl{plSongs = searchItem plSongs o s}
  filterItem  pl@PlaylistWidget{..} s   = pl{plSongs = filterItem plSongs   s}
  handleEvent    PlaylistWidget{plSongs = l, ..} ev =
    PlaylistWidget isMarked <$> time <*> case ev of
      EvPlaylistChanged -> MPDA.runCommand updatePlaylist

      EvCurrentSongChanged mSong -> do
        t <- currentTime
        let mIndex = mSong >>= MPD.sgIndex
            dt = t - plLastAction
        return $ if (10 < dt)
          then maybe l (ListWidget.setPosition l) mIndex
          else l

      EvDefaultAction -> do
        -- play selected song
        forM_ (ListWidget.select l >>= MPD.sgId) MPD.playId
        return l

      EvRemove -> do
        eval "copy"
        MPDA.runCommand $ do
          for_ (catMaybes $ map MPD.sgId $ ListWidget.selected l) MPDA.deleteId

        -- It is important to call `removeSelected` here, and not rely on
        -- EvPlaylistChanged, so that subsequent commands work on a current
        -- playlist!
        return (ListWidget.removeSelected l)

      EvPaste -> do
        (size, l_) <- paste (succ $ ListWidget.getPosition l)
        return $ (if 0 < size then ListWidget.moveDown else id) l_

      EvPastePrevious -> do
        (size, l_) <- paste (ListWidget.getPosition l)
        return $ ListWidget.move (negate size) l_

      _ -> songListHadler l ev
    where
      paste :: Int -> Vimus (Int, ListWidget MPD.Song)
      paste n = do
        songs <- readCopyRegister
        let size = length songs
        MPDA.runCommand $
          for_ (zip songs $ map Just [n..]) (uncurry MPDA.addId) *>
            -- It is important to call `updatePlaylist` here to prevent raise
            -- conditions between EvPlaylistChanged and subsequent commands!
            ((size,) <$> updatePlaylist)

      -- compare songs by playlist id
      eq = (==) `on` MPD.sgId

      updatePlaylist = do
        ListWidget.update eq l <$> MPDA.playlistInfo Nothing

      isMarked = case ev of
        EvCurrentSongChanged mSong -> maybe (const False) eq mSong
        _                          -> plMarked

      currentTime = liftIO getPOSIXTime
      keep = return plLastAction
      time = case ev of
        EvCurrentSongChanged {} -> keep
        EvPlaylistChanged    {} -> keep
        EvLibraryChanged     {} -> keep
        EvResize             {} -> keep
        EvLogMessage         {} -> keep
        EvDefaultAction      {} -> currentTime
        EvMoveUp             {} -> currentTime
        EvMoveDown           {} -> currentTime
        EvMoveAlbumPrev      {} -> currentTime
        EvMoveAlbumNext      {} -> currentTime
        EvMoveIn             {} -> currentTime
        EvMoveOut            {} -> currentTime
        EvMoveFirst          {} -> currentTime
        EvMoveLast           {} -> currentTime
        EvScroll             {} -> currentTime
        EvVisual             {} -> currentTime
        EvNoVisual           {} -> currentTime
        EvAdd                {} -> currentTime
        EvRemove             {} -> currentTime
        EvCopy               {} -> currentTime
        EvPaste              {} -> currentTime
        EvPastePrevious      {} -> currentTime

newtype LibraryWidget = LibraryWidget (ListWidget MPD.Song)

instance Widget LibraryWidget where
  render (LibraryWidget w)         = render w
  currentItem (LibraryWidget w)    = Song <$> ListWidget.select w
  searchItem (LibraryWidget w) o t = LibraryWidget (searchItem w o t)
  filterItem (LibraryWidget w) t   = LibraryWidget (filterItem w t)
  handleEvent (LibraryWidget l) ev = LibraryWidget <$> case ev of
    EvLibraryChanged songs -> do
      let eq = (==) `on` MPD.sgFilePath
      return $ ListWidget.update eq l (foldr consSong [] songs)

    EvDefaultAction -> do
      -- add selected song to playlist, and play it
      forM_ (ListWidget.select l) $ \song ->
        MPD.addId (MPD.sgFilePath song) Nothing >>= MPD.playId
      return l

    _ -> songListHadler l ev
    where
      consSong x xs = case x of
        MPD.LsSong song -> song : xs
        _               ->        xs

songListHadler :: ListWidget MPD.Song -> Event -> Vimus (ListWidget MPD.Song)
songListHadler l ev = case ev of
  EvMoveAlbumNext -> do
    case ListWidget.select l of
      Just song -> return (ListWidget.moveDownWhile (sameAlbum song) l)
      Nothing   -> return l

  EvMoveAlbumPrev -> do
    case ListWidget.select $ ListWidget.moveUp l of
      Just song -> return (ListWidget.moveUpWhile (sameAlbum song) l)
      Nothing   -> return l

  EvAdd -> do
    let items = ListWidget.selected l
    MPDA.runCommand $ for_ items (MPDA.add . MPD.sgFilePath)
    postAdd (length items) l

  EvCopy -> do
    writeCopyRegister $ pure (map MPD.sgFilePath $ ListWidget.selected l)
    handleEvent l EvNoVisual

  _ -> handleEvent l ev
  where
    sameAlbum a b = getAlbums a == getAlbums b && sgDirectory a == sgDirectory b
      where
        sgDirectory = dropFileName . MPD.toString . MPD.sgFilePath
        getAlbums = fromMaybe [] . Map.lookup MPD.Album . MPD.sgTags

postAdd :: (Searchable a, Renderable a) => Int -> ListWidget a -> Vimus (ListWidget a)
postAdd n l =
  -- Note: This behaves correctly for both
  --
  --  * if one item is selected
  --  * and the cursor is on a single item (:novisual)
  --
  --  (if one item is selected, it stays on the current song.  In :novisual it
  --  moves down).
  --
  --  But this is not obvious and may easily break.
  --
  --  FIXME: Do we want to introduce test cases at this point?
  handleEvent ((if n == 1 then ListWidget.moveDown else id) l) EvNoVisual

newtype BrowserWidget = BrowserWidget (ListWidget Content)

instance Widget BrowserWidget where
  render (BrowserWidget w)         = render w
  currentItem (BrowserWidget w)    = ListWidget.select w
  searchItem (BrowserWidget w) o t = BrowserWidget (searchItem w o t)
  filterItem (BrowserWidget w) t   = BrowserWidget (filterItem w t)

  handleEvent (BrowserWidget l) ev = BrowserWidget <$> case ev of

    -- FIXME: Can we construct a data structure from `songs_` and use this for
    -- the browser instead of doing MPD.lsInfo on every EvMoveIn?
    EvLibraryChanged _ {- songs_ -} -> do
      songs <- MPD.lsInfo ""
      let new = ListWidget.update (==) l (map toContent songs)
      moveInMany (ListWidget.breadcrumbs l) new

    EvDefaultAction -> do
      case ListWidget.select l of
        Just item -> case item of
          Dir   _         -> moveIn l
          PList _         -> moveIn l
          Song song       -> MPD.addId (MPD.sgFilePath song) Nothing >>= MPD.playId >> return l
          PListSong p i _ -> addPlaylistSong p i >>= MPD.playId >> return l
        Nothing -> return l

    EvMoveIn -> moveIn l

    EvMoveOut -> do
      case ListWidget.getParent l of
        Just p  -> return p
        Nothing -> return l

    EvAdd -> do
      -- FIXME: use Applicative style....
      let items = ListWidget.selected l
      forM_ items $ \item -> do
        case item of
          Dir   path      -> MPD.add path
          PList plst      -> MPD.load plst
          Song  song      -> MPD.add (MPD.sgFilePath song)
          PListSong p i _ -> void $ addPlaylistSong p i
      postAdd (length items) l

    EvCopy -> do
      writeCopyRegister . fmap concat . MPDA.runCommand $
        for (ListWidget.selected l) $ \item ->
          case item of
            Dir   path      -> MPDA.listAll path
            Song  song      -> pure [MPD.sgFilePath song]
            PList _         -> pure []
            PListSong _ _ _ -> pure []

      handleEvent l EvNoVisual

    _ -> handleEvent l ev
    where
      moveInMany :: [Content] -> ListWidget Content -> Vimus (ListWidget Content)
      moveInMany [] widget = return widget
      moveInMany (x:xs) widget = do
        case ListWidget.moveTo x widget of
          Just w -> if null xs
            then return w
            else moveIn w >>= moveInMany xs
          Nothing -> return widget

      moveIn :: ListWidget Content -> Vimus (ListWidget Content)
      moveIn w = case ListWidget.select w of
        Nothing -> return w
        Just item -> do
          case item of
            Dir path -> do
              new <- map toContent `fmap` MPD.lsInfo path
              return (ListWidget.newChild new w)
            PList path -> do
              new <- (zipWith (PListSong path) [0..]) `fmap` MPD.listPlaylistInfo path
              return (ListWidget.newChild new w)
            Song  _    -> return w
            PListSong  _ _ _ -> return w


newtype LogWidget = LogWidget (ListWidget LogMessage)

instance Widget LogWidget where
  render (LogWidget w)         = render w
  currentItem _                = Nothing
  searchItem (LogWidget w) o t = LogWidget (searchItem w o t)
  filterItem (LogWidget w) t   = LogWidget (filterItem w t)
  handleEvent (LogWidget widget) ev = LogWidget <$> case ev of
    EvLogMessage m -> return $ ListWidget.append widget m
    _              -> handleEvent widget ev


-- | Used for autocompletion.
autoComplete :: CompletionFunction
autoComplete = completeCommand commands

commands :: [Command]
commands = [
    command "help" "display a list of all commands, and their current keybindings" $ do
      macroGuesses <- Macro.guessCommands commandNames <$> getMacros
      addTab (Other "Help") (makeHelpWidget commands macroGuesses) AutoClose

  , command "log" "show the error log" $ do
      messages <- gets logMessages
      let widget = ListWidget.moveLast (ListWidget.new $ reverse messages)
      addTab (Other "Log") (AnyWidget . LogWidget $ widget) AutoClose

  , command "map" "display a list of all commands that are currently bound to keys" $ do
      showMappings

  , command "map" "display the command that is currently bound to the key {name}" $ do
      showMapping

  , command "map" [help|
        Bind the command {expansion} to the key {name}.  The same command may
        be bound to different keys.
        |] $ do
      addMapping

  , command "unmap" "remove the binding currently bound to the key {name}" $ do
      \(MacroName m) -> removeMacro m

  , command "mapclear" "" $ do
      clearMacros

  , command "exit" "exit vimus" $ do
      eval "quit"

  , command "quit" "exit vimus" $ do
      liftIO exitSuccess :: Vimus ()

  , command "close" "close the current window (not all windows can be closed)" $ do
      void closeTab

  , command "source" "read the file {path} and interprets all lines found there as if they were entered as commands." $ do
      \(Path p) -> liftIO (expandHome p) >>= either printError source_

  , command "runtime" "" $
      \(Path p) -> liftIO (getDataFileName p) >>= source_

  , command "color" "define the fore- and background color for a thing on the screen." $ do
      \color fg bg -> liftIO (defineColor color fg bg) :: Vimus ()

  , command "repeat" "set the playlist option *repeat*. When *repeat* is set, the playlist will start over when the last song has finished playing." $ do
      MPD.repeat  True :: Vimus ()

  , command "norepeat" "unset the playlist option *repeat*." $ do
      MPD.repeat  False :: Vimus ()

  , command "consume" "set the playlist option *consume*. When *consume* is set, songs that have finished playing are automatically removed from the playlist." $ do
      MPD.consume True :: Vimus ()

  , command "noconsume" "unset the playlist option *consume*" $ do
      MPD.consume False :: Vimus ()

  , command "random" "set the playlist option *random*. When *random* is set, songs in the playlist are played in random order." $ do
      MPD.random  True :: Vimus ()

  , command "norandom" "unset the playlist option *random*" $ do
      MPD.random  False :: Vimus ()

  , command "single" "" $ do
      MPD.single  True :: Vimus ()

  , command "nosingle" "" $ do
      MPD.single  False :: Vimus ()

  , command "volume" "[+-]<num> set volume to <num> or adjust by [+-] num" $ do
      volume :: Volume -> Vimus ()

 , command "toggle-repeat" "toggle the *repeat* option" $ do
      MPD.status >>= MPD.repeat  . not . MPD.stRepeat :: Vimus ()

  , command "toggle-consume" "toggle the *consume* option" $ do
      MPD.status >>= MPD.consume . not . MPD.stConsume :: Vimus ()

  , command "toggle-random" "toggle the *random* option" $ do
      MPD.status >>= MPD.random  . not . MPD.stRandom :: Vimus ()

  , command "toggle-single" "toggle the *single* option" $ do
      MPD.status >>= MPD.single  . not . MPD.stSingle :: Vimus ()

  , command "set-library-path" "While MPD knows where your songs are stored, vimus doesn't. If you want to use the *%* feature of the command :! you need to tell vimus where your songs are stored." $ do
      \(Path p) -> setLibraryPath p

  , command "next" "stop playing the current song, and starts the next one" $ do
      MPD.next :: Vimus ()

  , command "previous" "stop playing the current song, and starts the previous one" $ do
      MPD.previous :: Vimus ()

  , command "toggle" "toggle between play and pause" $ do
      MPDE.toggle :: Vimus ()

  , command "stop" "stop playback" $ do
      MPD.stop :: Vimus ()

  , command "update" "tell MPD to update the music database. You must update your database when you add or delete files in your music directory, or when you edit the metadata of a song.  MPD will only rescan a file already in the database if its modification time has changed." $ do
      void (MPD.update Nothing) :: Vimus ()

  , command "rescan" "" $ do
      void (MPD.rescan Nothing) :: Vimus ()

  , command "clear" "delete all songs from the playlist" $ do
      MPD.clear :: Vimus ()

  , command "search-next" "jump to the next occurrence of the search string in the current window" $
      searchNext

  , command "search-prev" "jump to the previous occurrence of the search string in the current window" $
      searchPrev


  , command "window-library" "open the *Library* window" $
      selectTab Library

  , command "window-playlist" "open the *Playlist* window" $
      selectTab Playlist

  , command "window-search" "open the *SearchResult* window" $
      selectTab SearchResult

  , command "window-browser" "open the *Browser* window" $
      selectTab Browser

  , command "window-next" "open the window to the right of the current one" $
      nextTab

  , command "window-prev" "open the window to the left of the current one" $
      previousTab

  , command "!" "execute {cmd} on the system shell. See chapter \"Using an external tag editor\" for an example." $
      runShellCommand

  , command "seek" "jump to the given position in the current song" $
      seek

  , command "visual" "start visual selection" $
      sendEventCurrent EvVisual

  , command "novisual" "cancel visual selection" $
      sendEventCurrent EvNoVisual

  -- Remove current song from playlist
  , command "remove" "remove the song under the cursor from the playlist" $
      sendEventCurrent EvRemove

  , command "paste" "add the last deleted song after the selected song in the playlist" $
      sendEventCurrent EvPaste

  , command "paste-prev" "" $
      sendEventCurrent EvPastePrevious

  , command "copy" "" $
      sendEventCurrent EvCopy

  -- Add given song to playlist
  , command "add" "append selected songs to the end of the playlist" $ do
      sendEventCurrent EvAdd

  -- insert a song right after the current song
  , command "insert" [help|
      inserts a song to the playlist. The song is inserted after the currently
      playing song.
      |] $
    withCurrentSong $ \song -> do -- FIXME: turn into an event
      st <- MPD.status
      case MPD.stSongPos st of
        Just n -> do
          -- there is a current song, add after
          _ <- MPD.addId (MPD.sgFilePath song) (Just . fromIntegral $ n + 1)
          sendEventCurrent EvMoveDown
        _                 -> do
          -- there is no current song, just add
          eval "add"

  -- Playlist: play selected song
  -- Library:  add song to playlist and play it
  -- Browse:   either add song to playlist and play it, or :move-in
  , command "default-action" [help|
      depending on the item under the cursor, somthing different happens:

      - *Playlist* start playing the song under the cursor

      - *Library* append the song under the cursor to the playlist and start playing it

      - *Browser* on a song: append the song to the playlist and play it. On a directory: go down to that directory.
      |] $ do
      sendEventCurrent EvDefaultAction

  , command "add-album" "add all songs of the album of the selected song to the playlist" $
    withCurrentSong $ \song -> do
      case Map.lookup MPD.Album $ MPD.sgTags song of
        Just albums -> do

          songs <- concat <$> mapM (MPD.find . (MPD.Album =?)) albums

          -- sort songs by disc/track
          let get :: MPD.Metadata -> MPD.Song -> Int
              get tag s = fromMaybe 0 $
                Map.lookup tag (MPD.sgTags s) >>= listToMaybe >>= readMaybe . MPD.toString

              sortByDisc  = comparing (get MPD.Disc)
              sortByTrack = comparing (get MPD.Track)
          MPDE.addMany "" $ map MPD.sgFilePath $ sortBy (sortByDisc `mappend` sortByTrack) songs

        Nothing -> printError "Song has no album metadata!"

  -- movement
  , command "move-up" "move the cursor one line up" $
      sendEventCurrent EvMoveUp

  , command "move-down" "move the cursor one line down" $
      sendEventCurrent EvMoveDown

  , command "move-album-prev" "move the cursor up to the first song of an album" $
      sendEventCurrent EvMoveAlbumPrev

  , command "move-album-next" "move the cursor down to the first song of an album" $
      sendEventCurrent EvMoveAlbumNext

  , command "move-in" "go down one level the directory hierarchy in the *Browser* window" $
      sendEventCurrent EvMoveIn

  , command "move-out" "go up one level in the directory hierarchy in the *Browser* window" $
      sendEventCurrent EvMoveOut

  , command "move-first" "go to the first line in the current window" $
      sendEventCurrent EvMoveFirst

  , command "move-last" "go to the last line in the current window" $
      sendEventCurrent EvMoveLast

  , command "scroll-up" "scroll the contents of the current window up one line" $
      sendEventCurrent (EvScroll (-1))

  , command "scroll-down" "scroll the contents of the current window down one line" $
      sendEventCurrent (EvScroll 1)

  , command "scroll-page-up" "scroll the contents of the current window up one page" $ do
      pageScroll >>= sendEventCurrent . EvScroll . negate

  , command "scroll-page-down" "scroll the contents of the current window down one page" $ do
      pageScroll >>= sendEventCurrent . EvScroll
  ]

getCurrentPath :: Vimus (Maybe FilePath)
getCurrentPath = do
  mBasePath <- getLibraryPath
  mPath <- withCurrentItem $ \item -> return . Just $ case item of
    Dir path        -> MPD.toString path
    PList l         -> MPD.toString l
    Song song       -> MPD.toString (MPD.sgFilePath song)
    PListSong _ _ s -> MPD.toString (MPD.sgFilePath s)

  return $ (mBasePath `append` mPath) <|> mBasePath
  where
    append = liftA2 (</>)


expandCurrentPath :: String -> Maybe String -> Either String String
expandCurrentPath s mPath = go s
  where
    go ""             = return ""
    go ('\\':'\\':xs) = ('\\':) `fmap` go xs
    go ('\\':'%':xs)  = ('%':)  `fmap` go xs
    go ('%':xs)       = case mPath of
                          Nothing -> Left "Path to music library is not set, hence % can not be used!"
                          Just p  -> (posixEscape p ++) `fmap` go xs
    go (x:xs)         = (x:) `fmap` go xs

-- | Evaluate command with given name
eval :: String -> Vimus ()
eval input = case parseCommand input of
  ("", "") -> return ()
  (c, args) -> case match c commandNames of
    None         -> printError $ printf "unknown command %s" c
    Match x      -> either printError id $ runAction (commandMap ! x) args
    Ambiguous xs -> printError $ printf "ambiguous command %s, could refer to: %s" c $ intercalate ", " xs

-- | A mapping from `commandName` to `commandAction`.
--
-- Actions with the same command name are combined with (<|>).
commandMap :: Map String VimusAction
commandMap = foldr f Map.empty commands
  where f c = Map.insertWith' (<|>) (commandName c) (commandAction c)

commandNames :: [String]
commandNames = Map.keys commandMap

-- | Run command with given name
runCommand :: String -> Vimus ()
runCommand c = eval c `catchError` (printError . show)

-- | Source file with given name.
--
-- TODO: proper error detection/handling
--
source :: String -> Vimus ()
source name = do
  rc <- lines `fmap` liftIO (readFile name)
  forM_ rc $ \line -> case strip line of

    -- skip empty lines
    ""    -> return ()

    -- skip comments
    '#':_ -> return ()

    -- run commands
    s     -> runCommand s

-- | A safer version of `source` that first checks whether the file exists.
source_ :: String -> Vimus ()
source_ name = do
  exists <- liftIO (doesFileExist name)
  if exists
    then do
      source name
    else do
      printError ("file " ++ show name ++ " not found")


------------------------------------------------------------------------
-- commands

newtype Path = Path String

instance Argument Path where
  argumentSpec = ArgumentSpec "path" noCompletion (Path <$> argumentParser)

newtype ShellCommand = ShellCommand String

instance Argument ShellCommand where
  argumentSpec = ArgumentSpec "cmd" noCompletion parser
    where
      parser = ShellCommand <$> do
        r <- takeInput
        when (null r) $ do
          missingArgument (undefined :: ShellCommand)
        return r

runShellCommand :: ShellCommand -> Vimus ()
runShellCommand (ShellCommand cmd) = (expandCurrentPath cmd <$> getCurrentPath) >>= either printError action
  where
    action s = liftIO $ do
      void endwin
      e <- system s
      case e of
        ExitSuccess   -> return ()
        ExitFailure n -> putStrLn ("shell returned " ++ show n)
      void getChar

newtype MacroName = MacroName String
  deriving (Eq, Show)


instance Argument MacroName where
  argumentSpec = ArgumentSpec "name" complete parser
    where
      parser = MacroName <$> (argumentParser >>= expandKeys_)

      -- a lifted version of expandKeys
      expandKeys_ :: String -> Parser String
      expandKeys_ = either (specificArgumentError . show) return . expandKeys

      complete input = case expandKeys input of
        Left (UnterminatedKeyReference k) -> (\x -> input ++ drop (length k) x) <$> completeOptions_ ">" keyNames k
        _                                 -> Left []

newtype MacroExpansion = MacroExpansion String

instance Argument MacroExpansion where
  argumentSpec = ArgumentSpec "expansion" noCompletion parser
    where
      parser = MacroExpansion <$> do
        e <- takeInput
        when (null e) $ do
          missingArgument (undefined :: MacroExpansion)

        either (specificArgumentError . show) return (expandKeys e)

showMappings :: Vimus ()
showMappings = do
  macroHelp <- Macro.helpAll <$> getMacros
  let helpWidget = AnyWidget $ ListWidget.new (sort macroHelp)
  addTab (Other "Mappings") helpWidget AutoClose

showMapping :: MacroName -> Vimus ()
showMapping (MacroName m) =
  getMacros >>= either printError printMessage . Macro.help m

-- | Add define a new mapping.
addMapping :: MacroName -> MacroExpansion -> Vimus ()
addMapping (MacroName m) (MacroExpansion e) = addMacro m e

newtype Seconds = Seconds MPD.Seconds

instance Argument Seconds where
  argumentSpec = ArgumentSpec "seconds" noCompletion parser
    where
      parser = Seconds <$> argumentParser

seek :: Seconds -> Vimus ()
seek (Seconds delta) = do
  st <- MPD.status
  let (current, total) = MPD.stTime st
  let newTime = round current + delta
  if (newTime < 0)
    then do
      -- seek within previous song
      case MPD.stSongPos st of
        Just currentSongPos -> unless (currentSongPos == 0) $ do
          previousItem <- MPD.playlistInfo $ Just (currentSongPos - 1)
          case previousItem of
            song : _ -> maybeSeek (MPD.sgId song) (MPD.sgLength song + newTime)
            _        -> return ()
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

-- | Volume argument for the 'volume' command.
data Volume =
    Volume Int        -- ^ Exact volume value, 0-100.
  | VolumeOffset Int  -- ^ Offset from current volume.
  deriving (Eq, Show)

instance Argument Volume where
  argumentSpec = ArgumentSpec "volume" noCompletion parseVolume

parseVolume :: Parser Volume
parseVolume = do
  r <- takeWhile1 (not . isSpace) <|> missingArgument proxy
  maybe (invalidArgument proxy r) return (readVolume r)
  where
    proxy = undefined :: Volume

readVolume :: String -> Maybe Volume
readVolume s = case s of
  ('+':n) -> VolumeOffset <$> offsetValue n
  ('-':_) -> VolumeOffset <$> offsetValue s
  _       -> Volume <$> volumeValue s
  where
    offsetValue x = readMaybe x >>= inRange (-100) 100
    volumeValue x = readMaybe x >>= inRange 0 100
    inRange l h x = guard (l <= x && x <= h) >> return x

-- | Set volume, or increment it by fixed amount.
volume :: Volume -> Vimus ()
volume (Volume v)       = MPD.setVolume v
volume (VolumeOffset i) = currentVolume >>= \v -> MPD.setVolume (adjust (v + i))
  where
    currentVolume = MPD.stVolume <$> MPD.status
    adjust x
      | x > 100   = 100
      | x < 0     = 0
      | otherwise = x
