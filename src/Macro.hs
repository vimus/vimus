{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Macro (
  Macros
, expandMacro
, defaultMacros
, addMacro
) where

import Data.Monoid
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (isInfixOf)
import UI.Curses

newtype Macros = Macros (Map String String)
  deriving Monoid

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

defaultMacros :: Macros
defaultMacros = Macros . Map.fromList $ zip (map macro macros) (map command macros)

macros :: [Macro]
macros = [
    Macro "q"     ":quit\n"
  , Macro "\3"    ":quit\n"
  , Macro "t"     ":toggle\n"

  , Macro "r"     ":toggle-repeat\n"
  , Macro "R"     ":toggle-random\n"
  , Macro "c"     ":toggle-consume\n"
  , Macro "s"     ":toggle-single\n"

  , Macro "k"        ":move-up\n"
  , Macro [keyUp]    ":move-up\n"
  , Macro "j"        ":move-down\n"
  , Macro [keyDown]  ":move-down\n"

  , Macro "h"        ":move-out\n"
  , Macro [keyLeft]  ":move-out\n"
  , Macro "l"        ":move-in\n"
  , Macro [keyRight] ":move-in\n"

  , Macro "G"     ":move-last\n"
  , Macro "gg"    ":move-first\n"
  , Macro "\25"   ":scroll-up\n"
  , Macro "\5"    ":scroll-down\n"

  , Macro "\2"        ":scroll-page-up\n"
  , Macro [keyPpage]  ":scroll-page-up\n"
  , Macro "\6"        ":scroll-page-down\n"
  , Macro [keyNpage]  ":scroll-page-down\n"

  , Macro [keyRight]  ":seek 5\n"
  , Macro [keyLeft]   ":seek -5\n"

  -- CTRL-N
  , Macro "\SO"   ":window-next\n"

  -- CTRL-P
  , Macro "\DLE"  ":window-prev\n"

  , Macro "1"     ":window-playlist\n"
  , Macro "2"     ":window-library\n"
  , Macro "3"     ":window-browser\n"
  , Macro "4"     ":window-search\n"
  , Macro "\n"    ":default-action\n"
  , Macro "d"     ":remove\n"
  , Macro "A"     ":add-album\n"
  , Macro "a"     ":add\n"
  , Macro "i"     ":insert\n"
  , Macro "n"     ":search-next\n"
  , Macro "N"     ":search-prev\n"
  ]
