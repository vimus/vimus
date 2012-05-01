{-# LANGUAGE OverloadedStrings, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Command (

  runCommand
, source
, search
, filter_

, createListWidget
, makeContentListWidget
, makeSongListWidget

, handlePlaylist
, handleLibrary
, handleBrowser
, autoComplete

-- * exported for testing
#ifdef TEST
, parseCommand
, autoComplete_
, MacroExpansion (..)
, ShellCommand (..)
#endif
) where

import           Data.List
import           Data.Char
import           Control.Monad (void, when, unless)
import           Control.Applicative
import           Data.Foldable (forM_)
import           Text.Printf (printf)
import           System.Exit
import           System.Cmd (system)

import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           Data.Map (Map, (!))
import qualified Data.Map as Map

import           Control.Monad.State.Strict (gets, get, modify, liftIO, MonadIO)
import           Control.Monad.Error (catchError)

import           Network.MPD ((=?))
import qualified Network.MPD as MPD hiding (withMPD)
import qualified Network.MPD.Commands.Extensions as MPDE

import           UI.Curses hiding (wgetch, ungetch, mvaddstr, err)

import           Paths_vimus (getDataFileName)

import           Util
import           Vimus
import           ListWidget (ListWidget, Renderable)
import qualified ListWidget
import           Content
import           WindowLayout
import           Key (expandKeys)
import qualified Macro
import           Input (CompletionFunction)
import           Command.Core
import           Command.Parser


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
  EvResize (y, _)  -> return . Just $ ListWidget.setTotalSize l y
  _                -> return Nothing

handlePlaylist :: Handler (ListWidget MPD.Song)
handlePlaylist ev l = case ev of
  EvPlaylistChanged songs -> do
    return $ Just $ ListWidget.update l songs

  EvCurrentSongChanged song -> do
    return $ Just $ l `ListWidget.setMarked` (song >>= MPD.sgIndex)

  EvRemove -> do
    eval "copy"
    forM_ (ListWidget.select l >>= MPD.sgId) MPD.deleteId
    return Nothing

  EvPaste -> do
    let n = succ (ListWidget.getPosition l)
    mPath <- gets copyRegister
    case mPath of
      Nothing -> return Nothing
      Just p  -> do
        MPDE.addIdMany p (Just . fromIntegral $ n)
        (return . Just . ListWidget.moveDown) l

  EvPastePrevious -> do
    let n = ListWidget.getPosition l
    mPath <- gets copyRegister
    forM_ mPath (`MPDE.addIdMany` (Just . fromIntegral) n)
    return Nothing

  _ -> handleList ev l

handleLibrary :: Handler (ListWidget MPD.Song)
handleLibrary ev l = case ev of
  EvLibraryChanged songs -> do
    return $ Just $ ListWidget.update l (foldr consSong [] songs)

  _ -> handleList ev l
  where
    consSong x xs = case x of
      MPD.LsSong song -> song : xs
      _               ->        xs

handleBrowser :: Handler (ListWidget Content)
handleBrowser ev l = case ev of

  -- FIXME: Can we construct a data structure from `songs_` and use this for
  -- the browser instead of doing MPD.lsInfo on every EvMoveIn?
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


createListWidget :: MonadIO m => Window -> [a] -> m (ListWidget a)
createListWidget window songs = liftIO $ do
  (viewSize, _) <- getmaxyx window
  return $ ListWidget.new songs viewSize

makeListWidget :: (Searchable a, Renderable a) => (ListWidget a -> Maybe Content) -> Handler (ListWidget a) -> ListWidget a -> Widget
makeListWidget select handle list = Widget {
    render      = ListWidget.render list
  , event       = \ev -> do
    -- handle events
    r <- handle ev list
    case r of
      Nothing -> return $ makeListWidget select handle list
      Just l  -> return $ makeListWidget select handle l

  , currentItem = select list
  , searchItem  = \order term ->
      makeListWidget select handle $ (searchFun order) (searchPredicate term) list
  , filterItem  = \term ->
      makeListWidget select handle $ ListWidget.filter (filterPredicate term) list
  }

searchFun :: SearchOrder -> (a -> Bool) -> ListWidget a -> ListWidget a
searchFun Forward  = ListWidget.search
searchFun Backward = ListWidget.searchBackward

makeContentListWidget :: Handler (ListWidget Content) -> ListWidget Content -> Widget
makeContentListWidget = makeListWidget ListWidget.select

makeSongListWidget :: Handler (ListWidget MPD.Song) -> ListWidget MPD.Song -> Widget
makeSongListWidget = makeListWidget (fmap Song . ListWidget.select)

-- | Used for autocompletion.
autoComplete :: CompletionFunction
autoComplete = autoComplete_ commandNames

autoComplete_ :: [String] -> CompletionFunction
autoComplete_ names input = case filter (isPrefixOf input) names of
  [x] -> Right (x ++ " ")
  xs  -> case commonPrefix $ map (drop $ length input) xs of
    "" -> Left xs
    ys -> Right (input ++ ys)

commands :: [Command]
commands = [

    command  "help"               $ do

      macroGuesses <- Macro.guessCommands commandNames <$> getMacros

      let help c = printf ":%-39s" (commandHelp c) ++ macros
            where
              -- macros defined for this command
              macros = maybe "" (intercalate "  " . map formatMacro) mMacros
              mMacros = Map.lookup (commandName c) macroGuesses

              formatMacro :: String -> String
              formatMacro = printf "%-10s"

      window <- gets mainWindow
      helpWidget <- createListWidget window (map help commands)
      addTab (Other "Help") (makeListWidget (const Nothing) handleList helpWidget) AutoClose

  , command  "log" $ do
      window <- gets mainWindow
      messages <- gets logMessages
      widget <- ListWidget.moveLast <$> createListWidget window (reverse messages)

      let handleLog ev l = case ev of
            EvLogMessage -> Just . ListWidget.update l . reverse <$> gets logMessages
            _            -> handleList ev l

      addTab (Other "Log") (makeListWidget (const Nothing) handleLog widget) AutoClose

  , command  "map"                $ showMappings
  , command  "map"                $ showMapping
  , command  "map"                $ addMapping
  , command  "unmap"              $ \(MacroName m) -> removeMacro m
  , command  "mapclear"           $ clearMacros

  , command  "exit"               $ (liftIO exitSuccess :: Vimus ())
  , command  "quit"               $ (liftIO exitSuccess :: Vimus ())
  , command  "close"              $ void closeTab
  , command  "source"             $ \(Path p) -> liftIO (expandHome p)      >>= either printError source_
  , command  "runtime"            $ \(Path p) -> liftIO (getDataFileName p) >>= source_

  , command3 "color"              $ \color fg bg -> liftIO (defineColor color fg bg)

  , command0 "repeat"             $ MPD.repeat  True
  , command0 "norepeat"           $ MPD.repeat  False
  , command0 "consume"            $ MPD.consume True
  , command0 "noconsume"          $ MPD.consume False
  , command0 "random"             $ MPD.random  True
  , command0 "norandom"           $ MPD.random  False
  , command0 "single"             $ MPD.single  True
  , command0 "nosingle"           $ MPD.single  False

  , command0 "toggle-repeat"      $ MPD.status >>= MPD.repeat  . not . MPD.stRepeat
  , command0 "toggle-consume"     $ MPD.status >>= MPD.consume . not . MPD.stConsume
  , command0 "toggle-random"      $ MPD.status >>= MPD.random  . not . MPD.stRandom
  , command0 "toggle-single"      $ MPD.status >>= MPD.single  . not . MPD.stSingle

  , command  "set-library-path"   $ \(Path p) -> setLibraryPath p

  , command0 "next"               $ MPD.next
  , command0 "previous"           $ MPD.previous
  , command0 "toggle"             $ MPDE.toggle
  , command0 "stop"               $ MPD.stop
  , command0 "update"             $ void (MPD.update Nothing)
  , command0 "rescan"             $ void (MPD.rescan Nothing)
  , command0 "clear"              $ MPD.clear
  , command  "search-next"        $ searchNext
  , command  "search-prev"        $ searchPrev

  , command  "window-library"     $ selectTab Library
  , command  "window-playlist"    $ selectTab Playlist
  , command  "window-search"      $ selectTab SearchResult
  , command  "window-browser"     $ selectTab Browser
  , command  "window-next"        $ nextTab
  , command  "window-prev"        $ previousTab

  , command  "!"                  $ runShellCommand

  , command  "seek"               $ seek

  -- Remove current song from playlist
  , command  "remove"             $ sendEventCurrent EvRemove
  , command  "paste"              $ sendEventCurrent EvPaste
  , command  "paste-prev"         $ sendEventCurrent EvPastePrevious
  , command  "copy" $ withCurrentItem $ \item -> do
      case item of
        Dir   path      -> copy path
        Song  song      -> (copy . MPD.sgFilePath) song
        PList _         -> return ()
        PListSong _ _ _ -> return ()

  -- Add given song to playlist
  , command  "add" $ withCurrentItem $ \item -> do
      case item of
        Dir   path      -> MPD.add path
        PList plst      -> MPD.load plst
        Song  song      -> MPD.add (MPD.sgFilePath song)
        PListSong p i _ -> void $ addPlaylistSong p i
      sendEventCurrent EvMoveDown

  -- Playlist: play selected song
  -- Library:  add song to playlist and play it
  -- Browse:   either add song to playlist and play it, or :move-in
  , command  "default-action" $ withCurrentItem $ \item -> do
      case item of
        Dir   _         -> eval "move-in"
        PList _         -> eval "move-in"
        Song  song      -> songDefaultAction song
        PListSong p i _ -> addPlaylistSong p i >>= MPD.playId

    -- insert a song right after the current song
  , command  "insert" $ withCurrentSong $ \song -> do -- FIXME: turn into an event
      st <- MPD.status
      case MPD.stSongPos st of
        Just n -> do
          -- there is a current song, add after
          _ <- MPD.addId (MPD.sgFilePath song) (Just . fromIntegral $ n + 1)
          sendEventCurrent EvMoveDown
        _                 -> do
          -- there is no current song, just add
          eval "add"

  , command  "add-album" $ withCurrentSong $ \song -> do
      case Map.lookup MPD.Album $ MPD.sgTags song of
        Just l -> do
          songs <- mapM MPD.find $ map (MPD.Album =?) l
          MPDE.addMany "" $ map MPD.sgFilePath $ concat songs
        Nothing -> printError "Song has no album metadata!"

  -- movement
  , command  "move-up"            $ sendEventCurrent EvMoveUp
  , command  "move-down"          $ sendEventCurrent EvMoveDown
  , command  "move-in"            $ sendEventCurrent EvMoveIn
  , command  "move-out"           $ sendEventCurrent EvMoveOut
  , command  "move-first"         $ sendEventCurrent EvMoveFirst
  , command  "move-last"          $ sendEventCurrent EvMoveLast
  , command  "scroll-up"          $ sendEventCurrent EvScrollUp
  , command  "scroll-down"        $ sendEventCurrent EvScrollDown
  , command  "scroll-page-up"     $ sendEventCurrent EvScrollPageUp
  , command  "scroll-page-down"   $ sendEventCurrent EvScrollPageDown
  ]

getCurrentPath :: Vimus (Maybe FilePath)
getCurrentPath = do
  mBasePath <- gets libraryPath
  mPath <- withCurrentWidget $ \widget -> do
    case currentItem widget of
      Just (Dir path)        -> (return . Just . MPD.toString) path
      Just (PList l)         -> (return . Just . MPD.toString) l
      Just (Song song)       -> (return . Just . MPD.toString . MPD.sgFilePath) song
      Just (PListSong _ _ s) -> (return . Just . MPD.toString . MPD.sgFilePath) s
      Nothing                -> return Nothing

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

parseCommand :: String -> (String, String)
parseCommand s = (name, dropWhile isSpace arg)
  where
    (name, arg) = case dropWhile isSpace s of
      '!':xs -> ("!", xs)
      xs     -> break isSpace xs

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
  argumentName = const "path"
  argumentParser = Path <$> argumentParser

newtype ShellCommand = ShellCommand String

instance Argument ShellCommand where
  argumentName   = const "cmd"
  argumentParser = ShellCommand <$> do
    r <- takeInput
    when (null r) $ do
      missingArgument (undefined :: ShellCommand)
    return r

runShellCommand :: ShellCommand -> Vimus ()
runShellCommand (ShellCommand cmd) = (expandCurrentPath cmd <$> getCurrentPath) >>= either printError action
  where
    action s = liftIO $ do
      endwin
      e <- system s
      case e of
        ExitSuccess   -> return ()
        ExitFailure n -> putStrLn ("shell returned " ++ show n)
      void getChar

newtype MacroName = MacroName String

instance Argument MacroName where
  argumentName = const "name"
  argumentParser = MacroName <$> do
    m <- takeWhile1 (not . isSpace)
    either specificArgumentError return (expandKeys m)

newtype MacroExpansion = MacroExpansion String

instance Argument MacroExpansion where
  argumentName = const "expansion"
  argumentParser = MacroExpansion <$> do
    e <- takeInput
    when (null e) $ do
      missingArgument (undefined :: MacroExpansion)
    either specificArgumentError return (expandKeys e)

showMappings :: Vimus ()
showMappings = do
  window <- gets mainWindow
  help <- Macro.helpAll <$> getMacros
  helpWidget <- createListWidget window (sort help)
  addTab (Other "Mappings") (makeListWidget (const Nothing) handleList helpWidget) AutoClose

showMapping :: MacroName -> Vimus ()
showMapping (MacroName m) = do
  ms <- getMacros
  either printError printMessage (expandKeys m >>= flip Macro.help ms)

-- | Add define a new mapping.
addMapping :: MacroName -> MacroExpansion -> Vimus ()
addMapping (MacroName m) (MacroExpansion e) = addMacro m e

newtype Seconds = Seconds MPD.Seconds

instance Argument Seconds where
  argumentName = const "seconds"
  argumentParser = Seconds <$> argumentParser

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

-- | Play song if on playlist, otherwise add it to the playlist and play it.
songDefaultAction :: MPD.Song -> Vimus ()
songDefaultAction song = case MPD.sgId song of
  -- song is already on the playlist
  Just i  -> MPD.playId i
  -- song is not yet on the playlist
  Nothing -> MPD.addId (MPD.sgFilePath song) Nothing >>= MPD.playId


------------------------------------------------------------------------
-- search

search :: String -> Vimus ()
search term = do
  modify $ \state -> state { getLastSearchTerm = term }
  search_ Forward term

filter_ :: String -> Vimus ()
filter_ term = withCurrentTab $ \tab -> do

  let searchResult = filterItem (tabContent tab) term
      closeMode = max Closeable (tabCloseMode tab)

  case tabName tab of
    SearchResult -> setCurrentWidget searchResult
    _            -> addTab SearchResult searchResult closeMode

searchNext :: Vimus ()
searchNext = do
  state <- get
  search_ Forward $ getLastSearchTerm state

searchPrev :: Vimus ()
searchPrev = do
  state <- get
  search_ Backward $ getLastSearchTerm state

search_ :: SearchOrder -> String -> Vimus ()
search_ order term = modifyCurrentWidget $ \widget ->
  return $ searchItem widget order term

data SearchPredicate = Search | Filter

searchPredicate :: Searchable a => String -> a -> Bool
searchPredicate = searchPredicate_ Search

filterPredicate :: Searchable a => String -> a -> Bool
filterPredicate = searchPredicate_ Filter

searchPredicate_ :: Searchable a => SearchPredicate -> String -> a -> Bool
searchPredicate_ predicate "" _ = onEmptyTerm predicate
  where
    onEmptyTerm Search = False
    onEmptyTerm Filter = True
searchPredicate_ _ term item = and $ map (\term_ -> or $ map (isInfixOf term_) tags) terms
  where
    tags = map (map toLower) (searchTags item)
    terms = words $ map toLower term
