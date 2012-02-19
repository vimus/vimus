{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Command (
  runCommand
, searchPredicate
, filterPredicate
, search
, commands

-- * exported for testing
, argumentErrorMessage
, parseCommand
, parseMapping
) where

import           Data.List
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Char
import           Text.Printf (printf)
import           System.Exit
import           System.Cmd (system)
import           Control.Monad.State (gets, get, modify, liftIO)
import           Control.Monad.Error (catchError)
import           Control.Monad
import           Control.Applicative

import           Network.MPD ((=?), Seconds)
import qualified Network.MPD as MPD hiding (withMPD)
import qualified Network.MPD.Commands.Extensions as MPDE
import           UI.Curses hiding (wgetch, ungetch, mvaddstr, err)

import           Vimus
import qualified ListWidget
import           Util (maybeRead, match, MatchResult(..), addPlaylistSong, posixEscape)
import           Content

import           System.FilePath (takeFileName, (</>))


command :: String -> (String -> Vimus ()) -> Command
command name action = Command name (Action action)

-- | Define a command that takes no arguments.
command0 :: String -> Vimus () -> Command
command0 name action = Command name (Action0 action)

-- | Define a command that takes one argument.
command1 :: String -> (String -> Vimus ()) -> Command
command1 name action = Command name (Action1 action)

-- | Define a command that takes two arguments.
command2 :: String -> (String -> String -> Vimus ()) -> Command
command2 name action = Command name (Action2 action)

commands :: [Command]
commands = [
    command0 "help"               $ setCurrentView Help
  , command  "map"                $ addMapping
  , command0 "exit"               $ liftIO exitSuccess
  , command0 "quit"               $ liftIO exitSuccess

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

  , command1 "set-library-path"   $ setLibraryPath

  , command0 "next"               $ MPD.next
  , command0 "previous"           $ MPD.previous
  , command0 "toggle"             $ MPDE.toggle
  , command0 "stop"               $ MPD.stop
  , command0 "update"             $ MPD.update []
  , command0 "clear"              $ MPD.clear
  , command0 "search-next"        $ searchNext
  , command0 "search-prev"        $ searchPrev
  , command0 "move-up"            $ modifyCurrentList ListWidget.moveUp
  , command0 "move-down"          $ modifyCurrentList ListWidget.moveDown
  , command0 "move-first"         $ modifyCurrentList ListWidget.moveFirst
  , command0 "move-last"          $ modifyCurrentList ListWidget.moveLast
  , command0 "scroll-up"          $ modifyCurrentList ListWidget.scrollUp
  , command0 "scroll-down"        $ modifyCurrentList ListWidget.scrollDown
  , command0 "scroll-page-up"     $ modifyCurrentList ListWidget.scrollPageUp
  , command0 "scroll-page-down"   $ modifyCurrentList ListWidget.scrollPageDown
  , command0 "window-library"     $ setCurrentView Library
  , command0 "window-playlist"    $ setCurrentView Playlist
  , command0 "window-search"      $ setCurrentView SearchResult
  , command0 "window-browser"     $ setCurrentView Browser
  , command0 "window-next"        $ nextView
  , command0 "window-prev"        $ previousView

  , command  "!"                  $ runShellCommand

  , command1 "seek" $ \s -> do
      let err = (printStatus $ "invalid argument: '" ++ s ++ "'!")
      maybe err seek (maybeRead s)

  -- Playlist: play selected song
  -- Library:  add song to playlist and play it
  -- Browse:   either add song to playlist and play it, or :move-in
  , command0 "default-action" $
      withCurrentItem $ \item -> do
        case item of
          Dir   _ -> eval "move-in"
          PList _ -> eval "move-in"
          Song  _ -> addCurrentSong >>? MPD.playId

    -- insert a song right after the current song
  , command0 "insert" $
      withCurrentSong $ \song -> do
        st <- MPD.status
        case MPD.stSongPos st of
          Just n -> do
            -- there is a current song, add after
            _ <- MPD.addId (MPD.sgFilePath song) (Just . fromIntegral $ n + 1)
            modifyCurrentSongList ListWidget.moveDown
          _                 -> do
            -- there is no current song, just add
            eval "add"

    -- Remove given song from playlist
  , command0 "remove" $
      withCurrentSong $ \song -> do
        case MPD.sgId song of
          (Just i) -> do MPD.deleteId i
          Nothing  -> return ()

  , command0 "add-album" $
      withCurrentSong $ \song -> do
        case Map.lookup MPD.Album $ MPD.sgTags song of
          Just l -> do
            songs <- mapM MPD.find $ map (MPD.Album =?) l
            MPDE.addMany "" $ map MPD.sgFilePath $ concat songs
          Nothing -> printStatus "Song has no album metadata!"

    -- Add given song to playlist
  , command0 "add" $
      withCurrentItem $ \item -> do
        case item of
          Dir   path -> MPD.add_ path
          PList plst -> MPD.load plst
          Song  _    -> addCurrentSong_
        modifyCurrentSongList ListWidget.moveDown

  -- Browse inwards/outwards
  , command0 "move-in" $
      withCurrentItem $ \item -> do
        case item of
          Dir   path -> MPD.lsInfo path >>= modifyCurrentSongList . ListWidget.newChild . map toContent
          PList list -> MPD.listPlaylistInfo list >>= modifyCurrentSongList . ListWidget.newChild . map Song
          Song  _    -> return ()

  , command0 "move-out" $
      modifyCurrentSongList $ \list -> do
        case ListWidget.getParent list of
          Just p  -> p
          Nothing -> list
  ]

getCurrentPath :: Vimus (Maybe FilePath)
getCurrentPath = do
  mBasePath <- gets libraryPath
  mPath <- withCurrentItem $ \item -> do
    case item of
      Dir path  -> return (Just path)
      PList l   -> return (Just l)
      Song song -> return (Just $ MPD.sgFilePath song)

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
      xs     -> span (not . isSpace) xs

-- | Evaluate command with given name
eval :: String -> Vimus ()
eval input = do
  case parseCommand input of
    ("", "") -> return ()
    (c, args) -> case match c $ Map.keys commandMap of
      None         -> printStatus $ printf "unknown command %s" c
      Match x      -> runAction args (commandMap ! x)
      Ambiguous xs -> printStatus $ printf "ambiguous command %s, could refer to: %s" c $ intercalate ", " xs

runAction :: String -> Action -> Vimus ()
runAction s action =
  case action of
    Action  a -> a s
    Action0 a -> case args of
      [] -> a
      xs -> argumentError 0 xs

    Action1 a -> case args of
      [x] -> a x
      xs  -> argumentError 1 xs

    Action2 a -> case args of
      [x, y] -> a x y
      xs     -> argumentError 2 xs
  where
    args = words s

argumentError
  :: Int      -- ^ expected number of arguments
  -> [String] -- ^ actual arguments
  -> Vimus ()
argumentError n = printStatus . argumentErrorMessage n

argumentErrorMessage
  :: Int      -- ^ expected number of arguments
  -> [String] -- ^ actual arguments
  -> String
argumentErrorMessage n args =
  case drop n args of
    []  ->  reqMessage
    [x] -> "unexpected argument: " ++ x
    xs  -> "unexpected arguments: " ++ unwords xs
  where
    reqMessage
      | n == 1    = "one argument required"
      | n == 2    = "two arguments required"
      | n == 2    = "three arguments required"
      | otherwise = show n ++ " arguments required"

-- | Run command with given name
runCommand :: String -> Vimus ()
runCommand c = eval c `catchError` (printStatus . show) >> renderMainWindow

commandMap :: Map String Action
commandMap = Map.fromList $ zip (map commandName commands) (map commandAction commands)


------------------------------------------------------------------------
-- commands

runShellCommand :: String -> Vimus ()
runShellCommand arg = (expandCurrentPath arg <$> getCurrentPath) >>= either printStatus action
  where
    action s = liftIO $ do
      endwin
      e <- system s
      case e of
        ExitSuccess   -> return ()
        ExitFailure n -> putStrLn ("shell returned " ++ show n)
      void getChar

-- | Currently only <cr> is expanded to '\n'.
expandKeyReferences :: String -> String
expandKeyReferences s =
  case s of
    ""                 -> ""
    '<':'c':'r':'>':xs -> '\n':expandKeyReferences xs
    x:xs               ->    x:expandKeyReferences xs

parseMapping :: String -> (String, String)
parseMapping s =
  case span (not . isSpace) (dropWhile isSpace s) of
    (macro, expansion) -> (macro, (expandKeyReferences . dropWhile isSpace) expansion)

addMapping :: String -> Vimus ()
addMapping s = case parseMapping s of
  ("", "") -> printStatus "not yet implemented" -- TODO: print all mappings
  (_, "")  -> printStatus "not yet implemented" -- TODO: print mapping with given name
  (m, e)   -> addMacro m e

seek :: Seconds -> Vimus ()
seek delta = do
  st <- MPD.status
  let (current, total) = MPD.stTime st
  let newTime = round current + delta
  if (newTime < 0)
    then do
      -- seek within previous song
      case MPD.stSongPos st of
        Just currentSongPos -> do
          playlist <- gets playlistWidget
          let previousItem = ListWidget.selectAt playlist (currentSongPos - 1)
          case previousItem of
            Song song -> maybeSeek (MPD.sgId song) (MPD.sgLength song + newTime)
            _         -> return ()
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


-- Add a currently selected song, if any, in regards to playlists and cue sheets
addCurrentSong :: Vimus (Maybe MPD.Id)
addCurrentSong = withCurrentSongList $ \list -> do
  case ListWidget.select list of
    Just (Song song) -> case MPD.sgId song of
      -- song is already on the playlist
      Just i  -> return (Just i)
      -- song is not yet on the playlist
      Nothing -> case ListWidget.getParent list of
        -- This item has a parent
        Just p  -> case ListWidget.select p of
          -- The parent is a playlist, use special function
          Just (PList pl) -> Just `fmap` addPlaylistSong pl (ListWidget.getPosition list)
          -- The parent is not a playlist, add normally
          _               -> addnormal
        -- No parent, add normally
        Nothing -> addnormal
        where
          addnormal = Just `fmap` MPD.addId (MPD.sgFilePath song) Nothing
    -- No song currently selected
    _ -> return Nothing

addCurrentSong_ :: Vimus ()
addCurrentSong_ = addCurrentSong >> return ()

-- Try on action on a command that may fail
(>>?) :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
a >>? b = do
  c <- a
  case c of
    Just r  -> b r
    Nothing -> return ()

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

searchPredicate :: String -> Content -> Bool
searchPredicate = searchPredicate_ False

filterPredicate :: String -> Content -> Bool
filterPredicate = searchPredicate_ True

searchPredicate_ :: Bool -> String -> Content -> Bool
searchPredicate_ onEmptyTerm "" _ = onEmptyTerm
searchPredicate_ _ term item = and $ map (\term_ -> or $ map (isInfixOf term_) tags) terms
  where
    tags :: [String]
    tags = map (map toLower) $ case item of
      Dir   path -> [takeFileName path]
      PList path -> [takeFileName path]
      Song  song -> concat $ Map.elems $ MPD.sgTags song
    terms = words $ map toLower term
