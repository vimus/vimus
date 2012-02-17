/* Customize the system-wide curses header. */

/* be C89 compliant */
#define NCURSES_ENABLE_STDBOOL_H 0

#define _XOPEN_SOURCE_EXTENDED
#define NCURSES_NOMACROS
#define NCURSES_OPAQUE 1

#include <ncursesw/ncurses.h>
#include <wchar.h>

