{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Key where

import           Data.Char (isControl)
import           UI.Curses

keyEsc = '\ESC'

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

-- | Convert given character to Vim's key-notation.
keyNotation c
  | c == keyEsc   = "<Esc>"
  | c == ctrlA    = "<C-A>"
  | c == ctrlB    = "<C-B>"
  | c == ctrlC    = "<C-C>"
  | c == ctrlD    = "<C-D>"
  | c == ctrlE    = "<C-E>"
  | c == ctrlF    = "<C-F>"
  | c == ctrlG    = "<C-G>"
  | c == ctrlN    = "<C-N>"
  | c == ctrlP    = "<C-P>"
  | c == ctrlY    = "<C-Y>"

  -- not defined here
  | c == '\n'     = "<CR>"

  | c == keyUp    = "<Up>"
  | c == keyDown  = "<Down>"
  | c == keyLeft  = "<Left>"
  | c == keyRight = "<Right>"

  | c == keyPpage = "<PageUp>"
  | c == keyNpage = "<PageDown>"

  | otherwise     = return c
