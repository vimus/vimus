{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Macro (
  Macros
, expandMacro
, addMacro
) where

import           Control.Monad

import           Data.Map (Map)
import qualified Data.Map as Map

import           Data.Default

import           Data.List (isInfixOf)
import           UI.Curses
import           Key

newtype Macros = Macros (Map String String)

data Macro = Macro {
  macro   :: String
, command :: String
}

expandMacro :: Monad m => Macros -> m Char -> (String -> m ()) -> String -> m ()
expandMacro (Macros macroMap) nextChar ungetstr = go
  where
    keys = Map.keys macroMap

    go input = do
      case Map.lookup input macroMap of
        Just v  -> ungetstr v
        Nothing -> unless (null matches) $ do
          c <- nextChar
          go (input ++ [c])
      where
        matches = filter (isInfixOf input) keys

addMacro :: String -> String -> Macros -> Macros
addMacro k v (Macros m) = Macros (Map.insert k v m)

instance Default Macros where
  def = Macros . Map.fromList $ zip (map macro macros) (map command macros)

macros :: [Macro]
macros = [
    Macro "q"         ":quit\n"
  , Macro [ctrlC]     ":quit\n"
  , Macro "c"         ":close\n"
  , Macro "t"         ":toggle\n"
  , Macro "r"         ":toggle-repeat\n"
  , Macro "R"         ":toggle-random\n"
  , Macro "C"         ":toggle-consume\n"
  , Macro "s"         ":toggle-single\n"
  , Macro "k"         ":move-up\n"
  , Macro [keyUp]     ":move-up\n"
  , Macro "j"         ":move-down\n"
  , Macro [keyDown]   ":move-down\n"
  , Macro "h"         ":move-out\n"
  , Macro [keyLeft]   ":move-out\n"
  , Macro "l"         ":move-in\n"
  , Macro [keyRight]  ":move-in\n"
  , Macro "G"         ":move-last\n"
  , Macro "gg"        ":move-first\n"
  , Macro [ctrlY]     ":scroll-up\n"
  , Macro [ctrlE]     ":scroll-down\n"
  , Macro [ctrlB]     ":scroll-page-up\n"
  , Macro [keyPpage]  ":scroll-page-up\n"
  , Macro [ctrlF]     ":scroll-page-down\n"
  , Macro [keyNpage]  ":scroll-page-down\n"
  , Macro [keyRight]  ":seek 5\n"
  , Macro [keyLeft]   ":seek -5\n"
  , Macro [ctrlN]     ":window-next\n"
  , Macro [ctrlP]     ":window-prev\n"
  , Macro "1"         ":window-playlist\n"
  , Macro "2"         ":window-library\n"
  , Macro "3"         ":window-browser\n"
  , Macro "4"         ":window-search\n"
  , Macro "\n"        ":default-action\n"
  , Macro "d"         ":remove\n"
  , Macro "A"         ":add-album\n"
  , Macro "a"         ":add\n"
  , Macro "i"         ":insert\n"
  , Macro "n"         ":search-next\n"
  , Macro "N"         ":search-prev\n"
  ]
