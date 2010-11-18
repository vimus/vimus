{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Vimus
( Vimus
, ProgramState (..)
, CurrentWindow (..)
, SongListWidget
, withCurrentWindow
, getCurrentWindow
)
where

import ListWidget (ListWidget)
import qualified Network.MPD as MPD hiding (withMPD)
import UI.Curses (Window)

import Network.MPD.Core
import Control.Monad.State

data CurrentWindow = Playlist | Library

type SongListWidget = ListWidget MPD.Song

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

type Vimus a = StateT ProgramState MPD a

{-
newtype Vimus a = Vimus {
  runVimus :: StateT ProgramState MPD a
} deriving (Monad, Functor, MonadIO, MonadState ProgramState, MonadError MPDError, MonadMPD)
-}


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
