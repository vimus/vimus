module Constant where

import Foreign.C.Types

#include "mycurses.h"

------------------------------------------------------------------------
-- ncurses(3NCURSES)
------------------------------------------------------------------------
err, ok :: CInt
ok  = (#const OK)
err = (#const ERR)

------------------------------------------------------------------------
-- color
------------------------------------------------------------------------

black, red, green, yellow, blue, magenta, cyan, white :: CShort
black   = (#const COLOR_BLACK)
red     = (#const COLOR_RED)
green   = (#const COLOR_GREEN)
yellow  = (#const COLOR_YELLOW)
blue    = (#const COLOR_BLUE)
magenta = (#const COLOR_MAGENTA)
cyan    = (#const COLOR_CYAN)
white   = (#const COLOR_WHITE)
