{-# LANGUAGE ForeignFunctionInterface #-}
module Curses (
  Status
, err
, ok
, module Misc
, module CursesInput
, module UI.Curses.Key
, addstr
, addnstr
, waddstr
, waddnstr
, mvaddstr
, mvaddnstr
, mvwaddstr
, mvwaddnstr

-- wchar functions
, mvwaddnwstr

-- TODO
, getmaxyx
, getbegyx
, stdscr
, refresh
, wrefresh
, initscr
, endwin
, isendwin
, getyx
, wdelch
, move
, werase
, wclrtoeol
, clrtoeol
, curs_set
, newwin
, delwin
, mvwin
, newpad
, prefresh
, mvwaddch
, waddch
-- , mytrace

-- inopts
, cbreak
, nocbreak
, echo
, noecho
, nl
, nonl
, halfdelay
, intrflush
, keypad
, meta
, nodelay
, raw
, noraw
, noqiflush
, qiflush
, notimeout
, timeout
, wtimeout
, Window
-- , typeahead
) where


import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.String (CString, CWString, newCWString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Data.Char (ord)

import Foreign.Marshal.Utils (toBool, fromBool)

import Data.ByteString.UTF8 as UTF8
import Data.ByteString.Char8 as Char8

import Misc hiding (cFromBool)
import CursesUtil
import UI.Curses.Key
import CursesInput


{# import UI.Curses.Type #}

#include "mycurses.h"

------------------------------------------------------------------------
-- default marshaller implementations
------------------------------------------------------------------------
withCString :: String -> (CString -> IO a) -> IO a
withCString = Char8.useAsCString . UTF8.fromString

------------------------------------------------------------------------
-- initscr(3NCURSES)
------------------------------------------------------------------------

-- NOTE: newterm, set_term and delscreen are not yet implemented
-- FIXME: initscr and newterm may be macros

{#fun unsafe initscr {} -> `Window' id#}
{#fun unsafe endwin {} -> `Status' toStatus*#}
{#fun unsafe isendwin {} -> `Bool'#}

------------------------------------------------------------------------
-- addstr(3NCURSES)
------------------------------------------------------------------------

-- FIXME: all of these routines except waddstr and waddnstr may be macros

{#fun unsafe addstr {`String'} -> `Status' toStatus*#}
{#fun unsafe addnstr {`String', `Int'} -> `Status' toStatus*#}
{#fun unsafe waddstr {id `Window', `String'} -> `Status' toStatus*#}
{#fun unsafe waddnstr {id `Window', `String', `Int'} -> `Status' toStatus*#}
{#fun unsafe mvaddstr {`Int', `Int', `String'} -> `Status' toStatus*#}
{#fun unsafe mvaddnstr {`Int', `Int', `String', `Int'} -> `Status' toStatus*#}
{#fun unsafe mvwaddstr {id `Window', `Int', `Int', `String'} -> `Status' toStatus*#}
{#fun unsafe mvwaddnstr {id `Window', `Int', `Int', `String', `Int'} -> `Status' toStatus*#}

------------------------------------------------------------------------
-- addwstr(3NCURSES)
------------------------------------------------------------------------
-- int addwstr(const wchar_t *wstr);
-- int addnwstr(const wchar_t *wstr, int n);
-- int waddwstr(WINDOW *win, const wchar_t *wstr);
-- int waddnwstr(WINDOW *win, const wchar_t *wstr, int n);
-- int mvaddwstr(int y, int x, const wchar_t *wstr);
-- int mvaddnwstr(int y, int x, const wchar_t *wstr, int n);
-- int mvwaddwstr(WINDOW *win, int y, int x, const wchar_t *wstr);
-- int mvwaddnwstr(WINDOW *win, int y, int x, const wchar_t *wstr, int n);
{#fun unsafe mvwaddnwstr as mvwaddnwstr_ {id `Window', `Int', `Int', castPtr `CWString', `Int'} -> `Status' toStatus*#}

mvwaddnwstr :: Window -> Int -> Int -> String -> Int -> IO Status
mvwaddnwstr win y x str n = newCWString str >>= \s -> mvwaddnwstr_ win y x s n

------------------------------------------------------------------------
-- refresh(3NCURSES)
------------------------------------------------------------------------

-- FIXME: refresh and redrawwin may be macros

{#fun unsafe refresh {} -> `Status' toStatus*#}
{#fun unsafe wrefresh {id `Window'} -> `Status' toStatus*#}
{#fun unsafe wnoutrefresh {id `Window'} -> `Status' toStatus*#}
{#fun unsafe doupdate {} -> `Status' toStatus*#}
{#fun unsafe redrawwin {id `Window'} -> `Status' toStatus*#}
{#fun unsafe wredrawln {id `Window', `Int', `Int'} -> `Status' toStatus*#}

------------------------------------------------------------------------
-- getyx(3NCURSES)
------------------------------------------------------------------------

{#fun unsafe nm_getyx as getyx {id `Window', alloca- `Int' peekInt*, alloca- `Int' peekInt*} -> `()'#}
{#fun unsafe nm_getparyx as getparyx {id `Window', alloca- `Int' peekInt*, alloca- `Int' peekInt*} -> `()'#}
{#fun unsafe nm_getbegyx as getbegyx {id `Window', alloca- `Int' peekInt*, alloca- `Int' peekInt*} -> `()'#}
{#fun unsafe nm_getmaxyx as getmaxyx {id `Window', alloca- `Int' peekInt*, alloca- `Int' peekInt*} -> `()'#}

peekInt :: Ptr CInt -> IO Int
peekInt = fmap fromIntegral . peek

#c
void nm_getyx(WINDOW* win, int* y, int* x);
void nm_getparyx(WINDOW *win, int* y, int* x);
void nm_getbegyx(WINDOW *win, int* y, int* x);
void nm_getmaxyx(WINDOW* win, int* y, int* x);
#endc

------------------------------------------------------------------------
-- TODO
------------------------------------------------------------------------

-- curscr
-- wtouchln
-- set_escdelay


------------------------------------------------------------------------
-- addch(3NCURSES)
------------------------------------------------------------------------
-- int addch(const chtype ch);
-- int waddch(WINDOW *win, const chtype ch);
{#fun unsafe waddch {id `Window', fromChar' `Char'} -> `Status' toStatus*#}

fromChar' :: Char -> Chtype_t
fromChar' = fromIntegral . ord

-- int mvaddch(int y, int x, const chtype ch);
-- int mvwaddch(WINDOW *win, int y, int x, const chtype ch);
{#fun unsafe mvwaddch {id `Window', `Int', `Int', fromChar' `Char'} -> `Status' toStatus*#}

-- int echochar(const chtype ch);
-- int wechochar(WINDOW *win, const chtype ch);



#c
WINDOW* get_stdscr(void);
#endc
stdscr :: Window
stdscr = {#call pure get_stdscr#}



cFromBool = fromBool


------------------------------------------------------------------------
-- inopts(3NCURSES)
------------------------------------------------------------------------

-- int cbreak(void);
{#fun unsafe cbreak   {} -> `Status' toStatus*#}

-- int nocbreak(void);
{#fun unsafe nocbreak {} -> `Status' toStatus*#}

-- int echo(void);
{#fun unsafe echo     {} -> `Status' toStatus*#}

-- int noecho(void);
{#fun unsafe noecho   {} -> `Status' toStatus*#}

-- int halfdelay(int tenths);
{#fun unsafe halfdelay {`Int'} -> `Status' toStatus*#}

-- int intrflush(WINDOW *win, bool bf);
{#fun unsafe intrflush {id `Window', `Bool'} -> `Status' toStatus*#}

-- int keypad(WINDOW *win, bool bf);
{#fun unsafe keypad   {id `Window', `Bool'} -> `Status' toStatus*#}

-- int meta(WINDOW *win, bool bf);
{#fun unsafe meta   {id `Window', `Bool'} -> `Status' toStatus*#}

-- int nodelay(WINDOW *win, bool bf);
{#fun unsafe nodelay   {id `Window', `Bool'} -> `Status' toStatus*#}

-- int raw(void);
{#fun unsafe raw      {} -> `Status' toStatus*#}

-- int noraw(void);
{#fun unsafe noraw    {} -> `Status' toStatus*#}

-- void noqiflush(void);
{#fun unsafe noqiflush {} -> `()'#}

-- void qiflush(void);
{#fun unsafe qiflush {} -> `()'#}

-- int notimeout(WINDOW *win, bool bf);
{#fun unsafe notimeout {id `Window', `Bool'} -> `Status' toStatus*#}

-- void timeout(int delay);
{#fun unsafe timeout {`Int'} -> `()'#}

-- void wtimeout(WINDOW *win, int delay);
{#fun unsafe wtimeout {id `Window', `Int'} -> `()'#}

-- int typeahead(int fd);
--{#fun unsafe typeahead {`Int'} -> `()'#}


------------------------------------------------------------------------
-- outopts(3NCURSES)
------------------------------------------------------------------------

{#fun unsafe nl {} -> `Status' toStatus*#}
{#fun unsafe nonl {} -> `Status' toStatus*#}
{#fun unsafe wdelch {id `Window'} -> `Status' toStatus*#}

{#fun unsafe move {`Int', `Int'} -> `Status' toStatus*#}

{#fun unsafe werase {id `Window'} -> `Status' toStatus*#}
{#fun unsafe wclrtoeol {id `Window'} -> `Status' toStatus*#}
{#fun unsafe clrtoeol {} -> `Status' toStatus*#}


-- |
-- Set the cursor state to invisible, normal, or very visible for visibility
-- equal to 0, 1, or 2 respectively.  Return the previous cursor state, if the
-- terminal supports the visibility requested; ERR otherwise.
--
-- TODO: handle ERR properly, use symbolic argument for visibility
{#fun unsafe curs_set {`Int'} -> `Int' fromIntegral#}


------------------------------------------------------------------------
-- window(3NCURSES)
------------------------------------------------------------------------

-- WINDOW *newwin(int nlines, int ncols, int begin_y, int begin_x);

{#fun unsafe newwin {`Int', `Int', `Int', `Int'} -> `Window' id#}

-- int delwin(WINDOW *win);
{#fun unsafe delwin {id `Window'} -> `Status' toStatus*#}

-- int mvwin(WINDOW *win, int y, int x);
{#fun unsafe mvwin {id `Window', `Int', `Int'} -> `Status' toStatus*#}



------------------------------------------------------------------------
-- pad(3NCURSES)
------------------------------------------------------------------------

-- WINDOW *newpad(int nlines, int ncols);
{#fun unsafe newpad {`Int', `Int'} -> `Window' id#}

-- WINDOW *subpad(WINDOW *orig, int nlines, int ncols, int begin_y, int begin_x);

-- int prefresh(WINDOW *pad, int pminrow, int pmincol, int sminrow, int smincol, int smaxrow, int smaxcol);
{#fun unsafe prefresh {id `Window', `Int', `Int', `Int', `Int', `Int', `Int'} -> `Status' toStatus*#}

-- int pnoutrefresh(WINDOW *pad, int pminrow, int pmincol, int sminrow, int smincol, int smaxrow, int smaxcol);

-- int pechochar(WINDOW *pad, chtype ch);

-- int pecho_wchar(WINDOW *pad, const cchar_t *wch);

{-
#c
void mytrace(void);
#endc

{#fun unsafe mytrace {} -> `()'#}
-}
