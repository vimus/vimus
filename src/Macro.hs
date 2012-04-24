{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}
module Macro (
  Macros
, addMacro
, removeMacro
, expandMacro
, help
, helpAll
, guessCommands
) where

import           Prelude hiding (getChar)
import           Control.Monad
import           Data.List (isInfixOf)
import           Text.Printf (printf)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Data.Default

import           Key (unExpandKeys)

import           Input

newtype Macros = Macros (Map String String)

-- helper for `help` and `helpAll`
formatMacro :: String -> String -> String
formatMacro m c = printf "%-10s %s" (unExpandKeys m) (unExpandKeys c)

-- | Get help message for a macro.
help :: String -> Macros -> Either String String
help m (Macros ms) = maybe (noMapping m) (Right . formatMacro m) (Map.lookup m ms)

-- | Convert macros to a list of strings, suitable for help.
helpAll :: Macros -> [String]
helpAll (Macros ms) = map (uncurry formatMacro) (Map.toList ms)

noMapping :: String -> Either String a
noMapping m = Left ("no mapping for " ++ unExpandKeys m)

-- | Expand a macro.
expandMacro :: Monad m => Macros -> String -> InputT m ()
expandMacro (Macros macroMap) = go
  where
    keys = Map.keys macroMap

    go input = do
      case Map.lookup input macroMap of
        Just v  -> unGetString v
        Nothing -> unless (null matches) $ do
          c <- getChar
          go (input ++ [c])
      where
        matches = filter (isInfixOf input) keys

-- | Add a macro.
addMacro :: String -> String -> Macros -> Macros
addMacro m e (Macros ms) = Macros (Map.insert m e ms)

-- | Remove a macro.
removeMacro :: String -> Macros -> Either String Macros
removeMacro m (Macros ms)
  | m `Map.member` ms  = (Right . Macros . Map.delete m) ms
  | otherwise          = noMapping m

-- | Construct a map from command to macros defined for that command.
guessCommands :: [String] -> Macros -> Map String [String]
guessCommands commands (Macros ms) = (Map.fromListWith (++) . foldr f [] . Map.toList) ms
  where
    f (m, e) xs
      | c `elem` commands = (c, [unExpandKeys m]) : xs
      | otherwise         = xs
      where c = strip e

    strip xs
      | ':':ys <- xs, '\n':zs <- reverse ys
        = reverse zs
      | otherwise = xs

-- | Default macros.
instance Default Macros where
  def = Macros Map.empty
