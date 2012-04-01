{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Key (
  expandKeys
, unExpandKeys
, keyEsc
, keyTab
, ctrlA
, ctrlB
, ctrlC
, ctrlD
, ctrlE
, ctrlF
, ctrlG
, ctrlN
, ctrlP
, ctrlY
) where

import           Data.Tuple (swap)
import           Data.Char (toLower)

import           Data.Map (Map)
import qualified Data.Map as Map

import           UI.Curses.Key


keyEsc = '\ESC'
keyTab = '\t'

ctrlA = '\SOH'
ctrlB = '\STX'
ctrlC = '\ETX'
ctrlD = '\EOT'
ctrlE = '\ENQ'
ctrlF = '\ACK'
ctrlG = '\BEL'
ctrlN = '\SO'
ctrlP = '\DLE'
ctrlY = '\EM'


-- | Associate each key with Vim's key-notation.
keys = [
    m keyEsc    "Esc"
  , m keyTab    "Tab"

  , m ctrlA     "C-A"
  , m ctrlB     "C-B"
  , m ctrlC     "C-C"
  , m ctrlD     "C-D"
  , m ctrlE     "C-E"
  , m ctrlF     "C-F"
  , m ctrlG     "C-G"
  , m ctrlN     "C-N"
  , m ctrlP     "C-P"
  , m ctrlY     "C-Y"

  -- not defined here
  , m '\n'      "CR"

  , m keyUp     "Up"
  , m keyDown   "Down"
  , m keyLeft   "Left"
  , m keyRight  "Right"

  , m keyPpage  "PageUp"
  , m keyNpage  "PageDown"
  ]
  where
    m = (,)


-- | A mapping from spcial keys to Vim's key-notation.
--
-- The brackets are included.
keyMap :: Map Char String
keyMap = Map.fromList (map (fmap (\s -> "<" ++ s ++ ">")) keys)


-- | A mapping from Vim's key-notation to their corresponding keys.
--
-- The brackets are not included, and only lower-case is used for key-notation.
keyNotationMap :: Map String Char
keyNotationMap = Map.fromList (map (swap . fmap (map toLower)) keys)


-- | Replace all special keys with their corresponding key reference.
--
-- Vim's key-notation is used for key references.
unExpandKeys = foldr f ""
  where
    f c
      -- escape opening brackets..
      | c == '<'  = ("\\<" ++)

      -- escape backslashes
      | c == '\\'  = ("\\\\" ++)

      | otherwise = (keyNotation c ++)

    -- | Convert given character to Vim's key-notation.
    keyNotation c = maybe (return c) id (Map.lookup c keyMap)


-- | Expand all key references to their corresponding keys.
--
-- Vim's key-notation is used for key references.
expandKeys :: String -> Either String String
expandKeys = go
  where
    go s = case s of
      ""        -> return ""

      -- keep escaped characters
      '\\':x:xs -> x `cons` go xs

      -- expand key references
      '<' : xs  -> expand xs

      -- keep any other characters
      x:xs      -> x `cons` go xs

    -- | Prepend given element to a list in the either monad.
    cons :: a -> Either String [a] -> Either String [a]
    cons = fmap . (:)

    -- Assume that `xs` starts with a key reference, terminated with a closing
    -- bracket.  Replace that key reference with it's corresponding key.
    expand xs = do
      (k, ys) <- takeKeyReference xs
      case Map.lookup k keyNotationMap of
        Just x -> (x :) `fmap` go ys
        Nothing -> Left $ "unknown key reference " ++ show k

    -- Assume that `s` starts with a key reference, terminated with a closing
    -- bracket.  Return the key reference (converted to lower-case) and the
    -- suffix, drop the closing bracket.
    takeKeyReference :: String -> Either String (String, String)
    takeKeyReference s = case break (== '>') s of
      ("",     _ ) -> Left "empty key reference"
      (xs,     "") -> Left ("unterminated key reference " ++ show xs)
      (xs, '>':ys) -> return (map toLower xs, ys)
      _            -> error "Key.takeKeyReference: this should never happen"
