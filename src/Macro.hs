module Macro (expandMacro) where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (isInfixOf)
import UI.Curses

import Vimus
import Command (runCommand)

data Macro = Macro {
    macro   :: String
  , command :: String
}

expandMacro :: Vimus Char -> String -> Vimus ()
expandMacro nextChar m = do
  case Map.lookup m macroMap of
    Just v  -> runCommand v
    Nothing -> unless (null matches) $ do
      c <- nextChar
      expandMacro nextChar (c : m)
  where
    keys   = Map.keys macroMap
    matches = filter (isInfixOf m) keys

macroMap :: Map String String
macroMap = Map.fromList $ zip (map macro macros) (map command macros)

macros :: [Macro]
macros = [
    Macro "q"         "quit"
  , Macro "\3"        "quit"
  , Macro "t"         "toggle"
  , Macro "k"         "move-up"
  , Macro [keyUp]     "move-up"
  , Macro "j"         "move-down"
  , Macro [keyDown]   "move-down"
  , Macro "G"         "move-last"
  , Macro "gg"        "move-first"
  , Macro "\25"       "scroll-up"
  , Macro "\5"        "scroll-down"
  , Macro "\2"        "scroll-page-up"
  , Macro [keyPpage]  "scroll-page-up"
  , Macro "\6"        "scroll-page-down"
  , Macro [keyNpage]  "scroll-page-down"
  , Macro [keyRight]  "seek-forward"
  , Macro [keyLeft]   "seek-backward"
  , Macro "\14"       "window-next"
  , Macro "1"         "window-playlist"
  , Macro "2"         "window-library"
  , Macro "3"         "window-search"
  , Macro "\n"        "play_"
  , Macro "d"         "remove"
  , Macro "A"         "add-album"
  , Macro "a"         "add"
  , Macro "i"         "insert"
  , Macro "n"         "search-next"
  , Macro "N"         "search-prev"
  ]
