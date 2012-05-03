{-# LANGUAGE OverloadedStrings, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-orphans #-} --FIXME: remove -fno-warn-orphans
module Command (

  runCommand
, source

, makePlaylistWidget
, makeLibraryWidget
, makeBrowserWidget

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

import           Control.Monad.State.Strict (gets, liftIO, MonadIO)
import           Control.Monad.Error (catchError)

import           Network.MPD ((=?))
import qualified Network.MPD as MPD hiding (withMPD)
import qualified Network.MPD.Commands.Extensions as MPDE

import           Data.Default

import           UI.Curses hiding (wgetch, ungetch, mvaddstr, err)

import           Paths_vimus (getDataFileName)

import           Util
import           Vimus
import           ListWidget (ListWidget, Renderable)
import qualified ListWidget
import           TextWidget (makeTextWidget)
import           Content
import           WindowLayout
import           Key (expandKeys)
import qualified Macro
import           Input (CompletionFunction)
import           Command.Core
import           Command.Parser


handleList :: Event -> ListWidget a -> Vimus (ListWidget a)
handleList ev l = case ev of
  EvMoveUp         -> return $ ListWidget.moveUp l
  EvMoveDown       -> return $ ListWidget.moveDown l
  EvMoveFirst      -> return $ ListWidget.moveFirst l
  EvMoveLast       -> return $ ListWidget.moveLast l
  EvScrollUp       -> return $ ListWidget.scrollUp l
  EvScrollDown     -> return $ ListWidget.scrollDown l
  EvScrollPageUp   -> return $ ListWidget.scrollPageUp l
  EvScrollPageDown -> return $ ListWidget.scrollPageDown l
  EvResize size    -> return $ ListWidget.resize l size
  _                -> return l

handlePlaylist :: Event -> ListWidget MPD.Song -> Vimus (ListWidget MPD.Song)
handlePlaylist ev l = case ev of
  EvPlaylistChanged songs -> do
    return $ ListWidget.update l songs

  EvCurrentSongChanged song -> do
    return $ l `ListWidget.setMarked` (song >>= MPD.sgIndex)

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
        MPDE.addIdMany p (Just . fromIntegral $ n)
        (return . ListWidget.moveDown) l

  EvPastePrevious -> do
    let n = ListWidget.getPosition l
    mPath <- gets copyRegister
    forM_ mPath (`MPDE.addIdMany` (Just . fromIntegral) n)
    return l

  _ -> event l ev

handleBrowser :: Event -> ListWidget Content -> Vimus (ListWidget Content)
handleBrowser ev l = case ev of

  -- FIXME: Can we construct a data structure from `songs_` and use this for
  -- the browser instead of doing MPD.lsInfo on every EvMoveIn?
  EvLibraryChanged _ {- songs_ -} -> do
    songs <- MPD.lsInfo ""
    return $ ListWidget.update l $ map toContent songs

  EvMoveIn -> flip (maybe $ return l) (ListWidget.select l) $ \item -> do
    case item of
      Dir path -> do
        new <- map toContent `fmap` MPD.lsInfo path
        return (ListWidget.newChild new l)
      PList path -> do
        new <- (map (uncurry $ PListSong path) . zip [0..]) `fmap` MPD.listPlaylistInfo path
        return (ListWidget.newChild new l)
      Song  _    -> return l
      PListSong  _ _ _ -> return l

  EvMoveOut -> do
    case ListWidget.getParent l of
      Just p  -> return p
      Nothing -> return l

  _ -> event l ev

instance (Searchable a, Renderable a) => Widget (ListWidget a) where
  render          = ListWidget.render
  event           = flip handleList
  currentItem     = const Nothing
  searchItem w o t = searchFun o (searchPredicate t) w
  filterItem w t   = ListWidget.filter (filterPredicate t) w

newtype PlaylistWidget = PlaylistWidget (ListWidget MPD.Song)

instance Widget PlaylistWidget where
  render (PlaylistWidget w)         = render w
  event  (PlaylistWidget w) ev      = PlaylistWidget <$> handlePlaylist ev w
  currentItem (PlaylistWidget w)    = Song <$> ListWidget.select w
  searchItem (PlaylistWidget w) o t = PlaylistWidget (searchItem w o t)
  filterItem (PlaylistWidget w) t   = PlaylistWidget (filterItem w t)

makePlaylistWidget :: AnyWidget
makePlaylistWidget = (AnyWidget . PlaylistWidget) (ListWidget.new [])

newtype LibraryWidget = LibraryWidget (ListWidget MPD.Song)

instance Widget LibraryWidget where
  render (LibraryWidget w)         = render w
  event  (LibraryWidget w) ev      = LibraryWidget <$> handleLibrary ev w

  currentItem (LibraryWidget w)    = fmap Song (ListWidget.select w)
  searchItem (LibraryWidget w) o t = LibraryWidget (searchItem w o t)
  filterItem (LibraryWidget w) t   = LibraryWidget (filterItem w t)

handleLibrary :: Event -> ListWidget MPD.Song -> Vimus (ListWidget MPD.Song)
handleLibrary ev l = case ev of
  EvLibraryChanged songs -> do
    return $ ListWidget.update l (foldr consSong [] songs)

  _ -> event l ev
  where
    consSong x xs = case x of
      MPD.LsSong song -> song : xs
      _               ->        xs


makeLibraryWidget :: AnyWidget
makeLibraryWidget = (AnyWidget . LibraryWidget) (ListWidget.new [])

newtype BrowserWidget = BrowserWidget (ListWidget Content)

instance Widget BrowserWidget where
  render (BrowserWidget w)         = render w
  event  (BrowserWidget w) ev      = BrowserWidget <$> handleBrowser ev w
  currentItem (BrowserWidget w)    = ListWidget.select w
  searchItem (BrowserWidget w) o t = BrowserWidget (searchItem w o t)
  filterItem (BrowserWidget w) t   = BrowserWidget (filterItem w t)

makeBrowserWidget :: AnyWidget
makeBrowserWidget = (AnyWidget . BrowserWidget) (ListWidget.new [])

newtype LogWidget = LogWidget (ListWidget LogMessage)

instance Widget LogWidget where
  render (LogWidget w)         = render w

  event (LogWidget widget) ev =
    LogWidget <$> case ev of
      EvLogMessage -> ListWidget.update widget . reverse <$> gets logMessages
      _            -> event widget ev
  currentItem _                = Nothing
  searchItem (LogWidget w) o t = LogWidget (searchItem w o t)
  filterItem (LogWidget w) t   = LogWidget (filterItem w t)

searchFun :: SearchOrder -> (a -> Bool) -> ListWidget a -> ListWidget a
searchFun Forward  = ListWidget.search
searchFun Backward = ListWidget.searchBackward


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
      addTab (Other "Help") (makeTextWidget (map help commands) 0) AutoClose

  , command  "log" $ do
      messages <- gets logMessages
      let widget = ListWidget.moveLast (ListWidget.new $ reverse messages)
      addTab (Other "Log") (AnyWidget . LogWidget $ widget) AutoClose

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
  help <- Macro.helpAll <$> getMacros
  let helpWidget = AnyWidget $ ListWidget.new (sort help)
  addTab (Other "Mappings") helpWidget AutoClose

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
