/* Customize the system-wide curses header. */

/* be C89 compliant */
#define NCURSES_ENABLE_STDBOOL_H 0

#define _XOPEN_SOURCE_EXTENDED
#define NCURSES_NOMACROS
#define NCURSES_OPAQUE 1

#ifdef ARCH
#include <ncurses.h>
#else
#include <ncursesw/ncurses.h>
#endif
#include <wchar.h>
