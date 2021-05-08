# vimus: An MPD client with vim-like key bindings

![screenshot of vimus](https://raw.github.com/vimus/vimus/master/screens/vimus.png "vimus awesomeness in pictures")

## Installing vimus

Get the latest version of [GHC](http://www.haskell.org/ghc/download_ghc_7_8_3)
or the [Haskell Platform](http://hackage.haskell.org/platform/) and make sure
that `~/.cabal/bin/` is on your `PATH`.

#### Install c2hs

    cabal update && cabal install c2hs


#### Get the source

    git clone https://github.com/vimus/vimus && cd vimus

#### Install and run vimus

    cabal install && vimus

Building vimus requires a wide character capable ncurses installation,
including headers.
If the build fails, make sure that the ncurses headers are available and
visible to Cabal, which may require you to install additional packages and/or
specify additional include and library directories.

##### Debian

To build ncursesw on Debian (and derived distributions), do

    apt-get install libncursesw5-dev
    cabal install --extra-include-dirs=/usr/include/ncursesw

##### MacOS

On MacOS you need to install ncursesw with Homebrew

    brew install ncursesw

## Using vimus

### Playing your first song

Read this when you have started vimus for the first time and are confused how
it works.

vimus uses [MPD](http://www.musicpd.org/) as it's backend, so you have to set
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

The file `$HOME/.config/vimus/vimusrc` is sourced on startup.  Lines starting with `#` are
comments.

### Using colors

You can use the `:color` command to customize colors.  For a light-on-dark
color scheme put the following into your `.config/vimus/vimusrc`.

    # a light-on-dark color scheme
    color tab green black
    color main cyan black
    color ruler green black
    color songstatus blue black
    color playstatus blue black
    color error white red
    color input white black
    color suggestions green black

### Using custom song formatting

You can use the `:song-format` command to customize song formatting. Song formats
consist of metaqueries:

  * `%artist%`
  * `%album%`
  * `%title%`
  * `%track%`
  * `%genre%`
  * `%year%`
  * `%composer%`
  * `%performer%`
  * `%comment%`
  * `%disc%`
  * `%length%`
  * `%filename%`
  * `%directory%`

and groupings, which can be nested:

    (%title%|%directory%/%filename%)

where `(` and `)` denote grouping borders and `|` separates alternatives.

The value of a grouping is the first alternative where all metaqueries have succeded.
A grouping without any metaqueries always succeeds.

The default format is:

	%artist% - %album% - %track% - %title%

*Note*: if a toplevel metaquery fails (i.e. not inside a grouping),
it's replaced with a `none` string.

### Adding custom keybindings

You can add keybindings with the `:map` command, e.g.:

    map q :quit<cr>

Have a look at the
[default keybindings](https://github.com/vimus/vimus/blob/master/resource/default-mappings).

### Emacs  keybindings

There are basic
[Emacs keybindings](https://github.com/vimus/vimus/blob/master/resource/emacs-mappings).
To enable them, add the following to your `.config/vimus/vimusrc`.

    runtime emacs-mappings

### Connecting to MPD

By default, Vimus tries to connect to MPD using a host of `localhost` on port 6600.

If you would like to use a different host or port, you can specify this in two ways:

* vimus first checks the `-h HOST` or `--host=HOST` command-line options for the host, and the `-P PORT` or `--port=PORT` command-line option for the port. To use a password, provide a value of the form `password@host` as the host.
* If the host is not provided as a command-line option, vimus falls back to the environment variables `MPD_HOST`. Similarly, if the port is not provided as a command-line option, it falls back to the environment variable `MPD_PORT`.
* If neither of these is given, vimus uses the default host of `localhost` and the default port at `6600`.

It is not currently possible to specify the host or port in your `.config/vimus/vimusrc`.

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

    $ ./ghci test/Spec.hs
    *Main> :main

(hack, hack, hack)

    *Main> :reload
    *Main> :main

### Resources

 * [libmpd documentation on Hackage](http://hackage.haskell.org/packages/archive/libmpd/latest/doc/html/Network-MPD.html)
 * [MPD protocol specification](http://www.musicpd.org/doc/protocol/)
