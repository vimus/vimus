#include "mycurses.h"


WINDOW* get_stdscr(void) {
    return stdscr;
}

int keyF(int n) {
    return KEY_F(n);
}

/* attr */
int color_pair(short n) {
    return COLOR_PAIR(n);
}

/***********************************************************************
 * getyx(3NCURSES)
 ***********************************************************************/
void nm_getyx(WINDOW* win, int* y, int* x) {
    getyx(win, *y, *x);
}

void nm_getparyx(WINDOW *win, int* y, int* x) {
    getparyx(win, *y, *x);
}

void nm_getbegyx(WINDOW *win, int* y, int* x) {
    getbegyx(win, *y, *x);
}

void nm_getmaxyx(WINDOW* win, int* y, int* x) {
    getmaxyx(win, *y, *x);
}


// FIXME: add proper binding to trace functions
/*
void mytrace(void) {
    trace(TRACE_VIRTPUT);
}
*/
