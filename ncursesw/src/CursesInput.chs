{-# LANGUAGE ForeignFunctionInterface #-}
module CursesInput where

import Foreign.C.Types
import Data.Char (chr, ord)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.Ptr (Ptr)

import CursesUtil
import qualified Constant
{# import UI.Curses.Type #}

#include "mycurses.h"

------------------------------------------------------------------------
-- getch(3NCURSES)
------------------------------------------------------------------------

-- TODO read manpage

-- int getch(void);
{#fun getch {} -> `Char' decodeKey#}

-- int wgetch(WINDOW *win);
{#fun wgetch {id `Window'} -> `Char' decodeKey#}


-- int mvgetch(int y, int x);
-- {#fun unsafe mvgetch {`Int', `Int'} -> `Char' decodeKey#}

-- int mvwgetch(WINDOW *win, int y, int x);
-- {#fun unsafe mvwgetch {id `Window', `Int', `Int'} -> `Char' decodeKey#}

-- int ungetch(int ch);
{#fun unsafe ungetch {fromChar `Char'} -> `Status' toStatus*#}

-- int has_key(int ch);
-- {#fun unsafe has_key {fromChar `Char'} -> `Bool'#}




-- getch returns ERR on various occasions, which is defined to -1.  As -1 has
-- no corresponding Char value, we map it to '\xffff'.  Unicode guarantees,
-- that '\xffff' is not a character at all.
not_a_character :: Char
not_a_character = '\xffff'

decodeKey :: CInt -> Char
decodeKey c = if c == Constant.err
    then not_a_character
    else chr $ fromIntegral c

fromChar :: Char -> CInt
fromChar = fromIntegral . ord

{#fun pure keyF {fromIntegral `Int'} -> `Char' decodeKey#}

#c
int keyF(int n);
#endc

-- wide character support

-- FIXME: right now the return type is IO (Status, Char), can we change that to
-- plain Char somehow?
{#fun get_wch  {alloca- `Char' peekChar*} -> `Status' toStatus*#}

{#fun wget_wch as wget_wch_ {id `Window', alloca- `Char' peekChar*} -> `Status' toStatus*#}

wget_wch :: Window -> IO Char
wget_wch window =  fmap snd $ wget_wch_ window

peekChar :: Ptr Wint_t -> IO Char
peekChar = fmap (chr . fromIntegral) . peek
