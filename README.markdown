# vimus: An MPD client with vim-like key bindings

![screenshot of vimus](https://raw.github.com/sol/vimus/master/screens/vimus.png "vimus awesomeness in pictures")

## Installing vimus

Get the latest release of the
[Haskell Platform](http://hackage.haskell.org/platform/),
and make sure that Cabal's `bindir` is on your `PATH`.

#### Install c2hs

    cabal update && cabal install c2hs


#### Get the source

    git clone https://github.com/sol/vimus
    cd vimus

#### Install ncursesw

    cd ncursesw && cabal install && cd ..

#### Install latest version of libmpd

    git clone https://github.com/sol/libmpd-haskell
    cd libmpd-haskell && cabal install && cd ..


#### Install and run vimus

    cabal install && vimus

## Using vimus

### Playing your first song

Read this when you have started vimus for the first time and are confused how
it works.

vimus uses [MPD](http://mpd.wikia.com/) as it's backend, so you have to set
that up first. If you are done doing so,

 1. Start vimus. If this is the first time you fired up vimus, you see an
    almost empty screen.  This is your current playlist, and there are no songs
    in it.  In the first line of your terminal, you see the words *Playlist*,
    *Library* and *Browser*.  These are the three main windows of vimus. You
    can access the main windows with the number keys `1`, `2` and `3`.

 2. Press the key `2`. This is your complete music library. If this window is
    empty, it means that you need to check your MPD setup.

 3. Use the keys `j` and `k` to position the cursor on one of the songs and
    press *Enter*. The song starts playing.

 4. Press the key `1` to return to the playlist. The playlist now contains one
    entry, the song that you have just started. On the bottom of your terminal,
    you see the name of the currently playing song.

### Managing the playlist

You can add songs and albums from your library to the playlist, and you can
remove songs from the playlist.

These are the possibilities to add songs to your playlist:

 1. Go to the library window by pressing `2`.

 2. Position your cursor on a song that you would like to hear.  You have three
    possibilities:

  - press `a` to append the song under the cursor to the end of the playlist.
  - press `i` to insert the song to the playlist. It will be played as soon as
    the current song has finished.
  - press `A` to append all songs of the album of the selected song to the
    playlist.

To remove a song from the playlist,

 1. Go to the playlist window by pressing `1`.

 2. Position the cursor on the song you want to remove from the playlist.

 3. Press the `d` key.

### Searching your music library

 1. Go to the library window by pressing `2`.

 2. Press `F`. The cursor jumps to the first song, and you are prompted to
    enter a filter term.

 3. Enter a word to filter your library, for example the name of an artist that
    you have music from. Note how vimus filters your music library as you type.

 4. Press *Enter*. A new window named *SearchResult* has opened, as you can see
    in the first line of your terminal. In this window, you can use the same
    keybindings as in the *Library* window to add songs or albums to your
    playlist.

 5. Press `F` again in the *SearchResult* window to enter a new search term and
    further narrow down the current search.

 6. Once you are done with this search, press `q` to close the window.

If you want to return to this window, use the number `4` key.

### Using the browser window

In the *Browser* window, you can navigate your music library based on the
directory structure which it has on your hard drive, as opposed to the
*Library* window, which relies on the music metadata.

 1. Press `3` to go to the browser window. In this window, directories are
    displayed in brackets [], and songs are displayed without brackets.

 2. Position your cursor on a directory and press `l` (the letter ELL) to go
    down into that directory.

 3. Use `h` to go up one directory level.

 4. Use `a` and `i` when the cursor is positioned on a song to add it to the
    playlist.

 5. Use `a` when the cursor is positioned on a directory to recursively add all
    songs in this directory.

### Command reference

To see a list of all available commands start vimus and type `:help` followed
by *Enter*.

This shows a list of all commands available in vimus, together with a short
description of what they do, and their default keybindings. For example the
command `:window-library` is bound to the key `2`.  This means that pressing
the key `2` or entering the command `:window-library` do the same thing.

You can define your own keybindings in the vimus configuration file. This is
described in chapter "Adding custom keybindings".

A keybinding of the form `<C-X>` means you have to press the *Control* key and
the `x` key at the same time. `<CR>` is the *Enter* key.

## Customizing  vimus

The file `$HOME/.vimusrc` is sourced on startup.  Lines starting with `#` are
comments.

### Using colors

You can use the `:color` command to customize colors.  For a light-on-dark
color scheme put the following into your `.vimusrc`.

    # a light-on-dark color scheme
    color tab green black
    color main cyan black
    color ruler green black
    color songstatus blue black
    color playstatus blue black
    color error white red
    color input white black
    color suggestions green black

### Adding custom keybindings

You can add keybindings with the `:map` command, e.g.:

    map q :quit<cr>

Have a look at the [default keybindings]
(https://github.com/sol/vimus/blob/master/resource/default-mappings).

### Emacs  keybindings

There are basic [Emacs keybindings]
(https://github.com/sol/vimus/blob/master/resource/emacs-mappings).  To enable
them, add the following to your `.vimusrc`.

    runtime emacs-mappings

### Recipes

#### Using an external tag editor

`:!` can be used to invoke external programs, `%` is expanded to the current
path (you need to set the base path to your library for `%` to work).

    set-library-path /path/to/music/directory

    # invoke kid3-qt with current song on 'T'
    map T :!kid3-qt %<cr>

## Getting help

`/join #vimus` on freenode!

## Development

### Running the test suite

    cabal configure --enable-tests && cabal build && cabal test

### Resources

 * [libmpd documentation on Hackage](http://hackage.haskell.org/packages/archive/libmpd/latest/doc/html/Network-MPD.html)
 * [MPD protocol specification](http://www.musicpd.org/doc/protocol/)
