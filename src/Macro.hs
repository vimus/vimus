{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Macro (
  Macros
, addMacro
, expandMacro
, help
, helpAll
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
help m (Macros ms) = maybe err (Right . formatMacro m) (Map.lookup m ms)
  where err = Left ("no mapping for " ++ show m)

-- | Convert macros to a list of strings, suitable for help.
helpAll :: Macros -> [String]
helpAll (Macros ms) = map (uncurry formatMacro) (Map.toList ms)

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
addMacro k v (Macros m) = Macros (Map.insert k v m)

-- | Default macros.
instance Default Macros where
  def = Macros Map.empty
