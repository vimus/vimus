module Macro (expandMacro) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (isInfixOf)

data Macro = Macro {
    macro   :: String
  , command :: String
}

expandMacro :: Monad m => (m Char) -> (String -> m ()) -> String -> m ()
expandMacro nextChar ungetstr m = do
  case Map.lookup m macroMap of
    Just v  -> ungetstr v
    Nothing -> if null matches then
        return ()
      else do
        c <- nextChar
        expandMacro nextChar ungetstr (c : m)
  where
    keys   = Map.keys macroMap
    matches = filter (isInfixOf m) keys

macroMap :: Map String String
macroMap = Map.fromList $ zip (map macro macros) (map command macros)

macros :: [Macro]
macros = [
    Macro "q"     ":quit\n"
  , Macro "\3"    ":quit\n"
  , Macro "t"     ":toggle\n"
  , Macro "k"     ":move-up\n"
  , Macro "j"     ":move-down\n"
  , Macro "G"     ":move-last\n"
  , Macro "gg"    ":move-first\n"
  , Macro "\25"   ":scroll-up\n"
  , Macro "\5"    ":scroll-down\n"
  , Macro "\2"    ":scroll-page-up\n"
  , Macro "\6"    ":scroll-page-down\n"
  , Macro "\14"   ":window-next\n"
  , Macro "1"     ":window-playlist\n"
  , Macro "2"     ":window-library\n"
  , Macro "\n"    ":play_\n"
  , Macro "d"     ":remove\n"
  , Macro "A"     ":add-album\n"
  , Macro "a"     ":add\n"
  , Macro "n"     ":search-next\n"
  , Macro "N"     ":search-prev\n"
  ]
