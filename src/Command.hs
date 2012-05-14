{-# LANGUAGE CPP, OverloadedStrings, QuasiQuotes #-}
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

import           Data.List
import           Data.Char
import           Control.Monad (void, when, unless, guard)
import           Control.Applicative
import           Data.Foldable (forM_)
import           Text.Printf (printf)
import           System.Exit
import           System.Cmd (system)

import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           Data.Map (Map, (!))
import qualified Data.Map as Map

import           Control.Monad.State.Strict (gets, liftIO, MonadIO)
import           Control.Monad.Error (catchError)

import           Network.MPD ((=?))
import qualified Network.MPD as MPD hiding (withMPD)
import qualified Network.MPD.Commands.Extensions as MPDE

import           UI.Curses hiding (wgetch, ungetch, mvaddstr, err)

import           Paths_vimus (getDataFileName)

import           Util
import           Vimus
import           Widget.ListWidget (ListWidget)
import qualified Widget.ListWidget as ListWidget
import           Widget.HelpWidget
import           Content
import           WindowLayout
import           Key (expandKeys)
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
    tab Playlist PlaylistWidget
  , tab Library  LibraryWidget
  , tab Browser  BrowserWidget
  ]
  where
    tab :: Widget w => TabName -> (ListWidget a -> w) -> Tab AnyWidget
    tab n t = Tab n (AnyWidget . t $ ListWidget.new []) Persistent

newtype PlaylistWidget = PlaylistWidget (ListWidget MPD.Song)

instance Widget PlaylistWidget where
  render (PlaylistWidget w)         = render w
  currentItem (PlaylistWidget w)    = Song <$> ListWidget.select w
  searchItem (PlaylistWidget w) o t = PlaylistWidget (searchItem w o t)
  filterItem (PlaylistWidget w) t   = PlaylistWidget (filterItem w t)
  handleEvent (PlaylistWidget l) ev = PlaylistWidget <$> case ev of
    EvPlaylistChanged songs -> do
      return $ ListWidget.update l songs

    EvCurrentSongChanged song -> do
      return $ l `ListWidget.setMarked` (song >>= MPD.sgIndex)

    EvDefaultAction -> do
      -- play selected song
      forM_ (ListWidget.select l >>= MPD.sgId) MPD.playId
      return l

    EvRemove -> do
      eval "copy"
      forM_ (ListWidget.select l >>= MPD.sgId) MPD.deleteId
      return l

    EvPaste -> do
      let n = succ (ListWidget.getPosition l)
      mPath <- gets copyRegister
      case mPath of
        Nothing -> return l
        Just p  -> do
          _ <- MPDE.addIdMany p (Just . fromIntegral $ n)
          (return . ListWidget.moveDown) l

    EvPastePrevious -> do
      let n = ListWidget.getPosition l
      mPath <- gets copyRegister
      forM_ mPath (`MPDE.addIdMany` (Just . fromIntegral) n)
      return l

    _ -> handleEvent l ev

newtype LibraryWidget = LibraryWidget (ListWidget MPD.Song)

instance Widget LibraryWidget where
  render (LibraryWidget w)         = render w
  currentItem (LibraryWidget w)    = fmap Song (ListWidget.select w)
  searchItem (LibraryWidget w) o t = LibraryWidget (searchItem w o t)
  filterItem (LibraryWidget w) t   = LibraryWidget (filterItem w t)
  handleEvent (LibraryWidget l) ev = LibraryWidget <$> case ev of
    EvLibraryChanged songs -> do
      return $ ListWidget.update l (foldr consSong [] songs)

    EvDefaultAction -> do
      -- add selected song to playlist, and play it
      forM_ (ListWidget.select l) $ \song ->
        MPD.addId (MPD.sgFilePath song) Nothing >>= MPD.playId
      return l

    _ -> handleEvent l ev
    where
      consSong x xs = case x of
        MPD.LsSong song -> song : xs
        _               ->        xs

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
      return $ ListWidget.update l $ map toContent songs

    EvDefaultAction -> do
      case ListWidget.select l of
        Just item -> case item of
          Dir   _         -> moveIn
          PList _         -> moveIn
          Song song       -> MPD.addId (MPD.sgFilePath song) Nothing >>= MPD.playId >> return l
          PListSong p i _ -> addPlaylistSong p i >>= MPD.playId >> return l
        Nothing -> return l

    EvMoveIn -> moveIn

    EvMoveOut -> do
      case ListWidget.getParent l of
        Just p  -> return p
        Nothing -> return l

    _ -> handleEvent l ev
    where
      moveIn = flip (maybe $ return l) (ListWidget.select l) $ \item -> do
        case item of
          Dir path -> do
            new <- map toContent `fmap` MPD.lsInfo path
            return (ListWidget.newChild new l)
          PList path -> do
            new <- (zipWith (PListSong path) [0..]) `fmap` MPD.listPlaylistInfo path
            return (ListWidget.newChild new l)
          Song  _    -> return l
          PListSong  _ _ _ -> return l


newtype LogWidget = LogWidget (ListWidget LogMessage)

instance Widget LogWidget where
  render (LogWidget w)         = render w
  currentItem _                = Nothing
  searchItem (LogWidget w) o t = LogWidget (searchItem w o t)
  filterItem (LogWidget w) t   = LogWidget (filterItem w t)
  handleEvent (LogWidget widget) ev = LogWidget <$> case ev of
    EvLogMessage -> ListWidget.update widget . reverse <$> gets logMessages
    _            -> handleEvent widget ev


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

  -- Remove current song from playlist
  , command "remove" "remove the song under the cursor from the playlist" $
      sendEventCurrent EvRemove

  , command "paste" "add the last deleted song after the selected song in the playlist" $
      sendEventCurrent EvPaste

  , command "paste-prev" "" $
      sendEventCurrent EvPastePrevious

  , command "copy" "" $
    withCurrentItem $ \item -> do
      case item of
        Dir   path      -> copy path
        Song  song      -> (copy . MPD.sgFilePath) song
        PList _         -> return ()
        PListSong _ _ _ -> return ()

  -- Add given song to playlist
  , command "add" "append a song or directory to the end of playlist" $
    withCurrentItem $ \item -> do
      case item of
        Dir   path      -> MPD.add path
        PList plst      -> MPD.load plst
        Song  song      -> MPD.add (MPD.sgFilePath song)
        PListSong p i _ -> void $ addPlaylistSong p i
      sendEventCurrent EvMoveDown

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

  , command "add-album" "add all songs of the album of the selected song to the playlist" $
    withCurrentSong $ \song -> do
      case Map.lookup MPD.Album $ MPD.sgTags song of
        Just l -> do
          songs <- mapM MPD.find $ map (MPD.Album =?) l
          MPDE.addMany "" $ map MPD.sgFilePath $ concat songs
        Nothing -> printError "Song has no album metadata!"

  -- movement
  , command "move-up" "move the cursor one line up" $
      sendEventCurrent EvMoveUp

  , command "move-down" "move the cursor one line down" $
      sendEventCurrent EvMoveDown

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
  argumentSpec = ArgumentSpec "name" noCompletion parser
    where
      parser = MacroName <$> (argumentParser >>= expandKeys_)

      -- a lifted version of expandKeys
      expandKeys_ :: String -> Parser String
      expandKeys_ = either specificArgumentError return . expandKeys

newtype MacroExpansion = MacroExpansion String

instance Argument MacroExpansion where
  argumentSpec = ArgumentSpec "expansion" noCompletion parser
    where
      parser = MacroExpansion <$> do
        e <- takeInput
        when (null e) $ do
          missingArgument (undefined :: MacroExpansion)
        either specificArgumentError return (expandKeys e)

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
          previousItem <- MPD.playlistInfo $ Just (currentSongPos - 1, 1)
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
    offsetValue x = maybeRead x >>= inRange (-100) 100
    volumeValue x = maybeRead x >>= inRange 0 100
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
