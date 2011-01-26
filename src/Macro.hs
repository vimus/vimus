module Macro (macroMap, prettyPrintKey) where

import Data.Map (Map)
import qualified Data.Map as Map

import UI.Curses

data Macro = Macro {
    macro   :: String
  , command :: String
}

macros :: [Macro]
macros = [
    Macro "q"         "quit"
  , Macro [ctrlC]     "quit"
  , Macro "t"         "toggle"
  , Macro "k"         "move-up"
  , Macro [keyUp]     "move-up"
  , Macro "j"         "move-down"
  , Macro [keyDown]   "move-down"
  , Macro "G"         "move-last"
  , Macro "gg"        "move-first"
  , Macro [ctrlY]     "scroll-up"
  , Macro [ctrlE]     "scroll-down"
  , Macro [ctrlB]     "scroll-page-up"
  , Macro [keyPpage]  "scroll-page-up"
  , Macro [ctrlF]     "scroll-page-down"
  , Macro [keyNpage]  "scroll-page-down"
  , Macro [keyRight]  "seek-forward"
  , Macro [keyLeft]   "seek-backward"
  , Macro [ctrlN]     "window-next"
  , Macro "1"         "window-playlist"
  , Macro "2"         "window-library"
  , Macro "3"         "window-search"
  , Macro [cr]        "play_"
  , Macro "d"         "remove"
  , Macro "A"         "add-album"
  , Macro "a"         "add"
  , Macro "i"         "insert"
  , Macro "n"         "search-next"
  , Macro "N"         "search-prev"
  ]

macroMap :: Map String String
macroMap = Map.fromList $ zip (map macro macros) (map command macros)

cr, ctrlC, ctrlN, ctrlF, ctrlB, ctrlE, ctrlY :: Char
cr    = '\n'
ctrlC = '\3'
ctrlN = '\14'
ctrlF = '\6'
ctrlB = '\2'
ctrlE = '\5'
ctrlY = '\25'

prettyPrintKey :: Char -> String
prettyPrintKey key
  | key == cr       = "<CR>"
  | key == ctrlC    = "CTRL-C"
  | key == ctrlN    = "CTRL-N"
  | key == ctrlF    = "CTRL-F"
  | key == ctrlB    = "CTRL-B"
  | key == ctrlE    = "CTRL-E"
  | key == ctrlY    = "CTRL-Y"
  | key == keyUp    = "<Up>"
  | key == keyDown  = "<Down>"
  | key == keyLeft  = "<Left>"
  | key == keyRight = "<Right>"
  | key == keyPpage = "<PageUp>"
  | key == keyNpage = "<PageDown>"
  | otherwise       = return key
