# vimus: An MPD client with vim-like key bindings

![screenshot of vimus](https://raw.github.com/sol/vimus/master/screens/vimus.png "vimus awesomeness in pictures")

## Installing

Get latest release of [The Haskell
Platform](http://hackage.haskell.org/platform/), and make sure that Cabal's
`bindir` is on your `PATH`.

#### Install c2hs

    cabal update && cabal install c2hs


#### Get the source

    git clone https://github.com/sol/vimus
    cd vimus

#### Install ncursesw

    cd ncursesw && cabal install && cd ..

Or alternatively, if you are using Arch Linux.

    cd ncursesw && cp src/mycurses.h.arch src/mycurses.h && cabal install && cd ..

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
    can access the main windows with the number keys *1*, *2* and *3*.

 2. Press the number *2* key. This is your complete music library. If this
    window is empty, it means that you need to check your MPD setup.

 3. Use the keys *j* and *k* to position the cursor on one of the songs and
    press *enter*. The song starts playing.

 4. Press the key *1* to return to the playlist. The playlist now contains one
    entry, the song that you have just started. On the bottom of your terminal,
    you see the name of the currently playing song.

### Managing the playlist

You can add songs and albums from your library to the playlist, and you can
remove songs from the playlist.

These are the possibilities to add songs to your playlist:

 1. Go to the library window by pressing *2*.

 2. Position your cursor on a song that you would like to hear.  You have three
    possibilities:

  - press *a* to append the song under the cursor to the end of the playlist.
  - press *i* to insert the song to the playlist. It will be played as soon as
    the current song has finished.
  - press *A* to append all songs of the album of the selected song to the
    playlist.

To remove a song from the playlist,

 1. Go to the playlist window by pressing *1*.

 2. Position the cursor on the song you want to remove from the playlist.

 3. Press the *d* key.

### Searching your music library

 1. Go to the library window by pressing *2*.

 2. Press *F*. The cursor jumps to the last line in the terminal, and you are
    prompted to enter a filter term.

 3. Enter a word to filter your library, for example the name of an artist that
    you have music from. Note how vimus filters your music library as you type.

 4. Press *enter*. A new window named *SearchResult* has opened, as you can see
    in the first line of your terminal. In this window, you can use the same
    keybindings as in the *Library* window to add songs or albums to your
    playlist.

 5. Press *F* again in the *SearchResult* window to enter a new search term and
    further narrow down the current search.

 6. Once you are done with this search, press *q* to close the window.

If you want to return to this window, use the number *4* key.

### Using the browser window

In the *Browser* window, you can navigate your music library based on the
directory structure which it has on your hard drive, as opposed to the
*Library* window, which relies on the music metadata.

 1. Press *3* to go to the browser window. In this window, directories are
    displayed in brackets [], and songs are displayed without brackets.

 2. Position your cursor on a directory and press *l* (the letter ELL) to go
    down into that directory.

 3. Use *h* to go up one directory level.

 4. Use *a* and *i* when the cursor is positioned on a song to add it to the
    playlist.

 5. Use *a* when the cursor is positioned on a directory to recursively add all
    songs in this directory.

### Command reference

This is a list of all commands available in vimus, together with a short
description of what they do, and their default keybindings. For example the
command `:window-library` is bound to the key *2*.  This means that pressing
the key *2* or entering the command `:window-library` do the same thing.

You can define your own keybindings in the vimus configuration file. This is
described in chapter "Add custom mappings".

A keybinding of the form &lt;C-X> means you have to press the Control key and
the x key at the same time. &lt;CR> is the *enter* key

- `:help` displays a list of all commands, and their current keybindings
- `:log`
- `:map` displays a list of all commands that are currently bound to keys
- `:map {name}` displays the command that is currently bound to the key {name}
- `:map {name} {expansion}` binds the command {expansion} to the key {name}.
  The same command may be bound to different keys.
- `:unmap {name}` removes the binding currently bound to the key {name}
- `:exit` exits vimus
- `:quit` &lt;C-C> exits vimus
- `:close` q closes the current window (not all windows can be closed)
- `:source {path}` reads the file {path} and interprets all lines found there
  as if they were entered as commands.
- `:runtime {path}`
- `:color {item} {color} {color}` defines the background- and foreground color
  for a thing on the screen. See chapter "Using colors" for examples.
- `:repeat` sets the playlist option *repeat*. When *repeat* is set, the
  playlist will start over when the last song has finished playing.
- `:norepeat` unsets the playlist option *repeat*.
- `:consume` sets the playlist option *consume*. When *consume* is set, songs
  that have finished playing are automatically removed from the playlist.
- `:noconsume` unsets the playlist option *consume*
- `:random` sets the playlist option *random*. When *random* is set, songs in
  the playlist are played in random order.
- `:norandom` unsets the playlist option *random*
- `:single`
- `:nosingle`
- `:toggle-repeat` r toggles the *repeat* option
- `:toggle-consume` c toggles the *consume* option
- `:toggle-random` R toggles the *random* option
- `:toggle-single` s toggles the *single* option
- `:set-library-path {path}` While MPD knows where your songs are stored, vimus
  doesn't. If you want to use the *%* feature of the command :! you need to
  tell vimus where your songs are stored. See also chapter "Using an external
  tag editor".
- `:next` stops playing the current song, and starts the next one
- `:previous` stops playing the current song, and starts the previous one
- `:toggle` t toggles between playback and pause of the current song
- `:stop` stops playback
- `:update` tells MPD to update the music database. You must update your
  database when you add or delete files in your music directory, or when you
  edit the metadata of a song.  MPD will only rescan a file already in the
  database if its modification time has changed.
- `:rescan`
- `:clear` deletes all songs from the playlist
- `:search-next` n jumps to the next occurrence of the search string in the
  current window
- `:search-prev` N jumps to the previous occurrence of the search string in the
  current window
- `:window-library` 2 opens the *Library* window
- `:window-playlist` 1 opens the *Playlist* window
- `:window-search` 4 opens the *SearchResult* window
- `:window-browser` 3 opens the *Browser* window
- `:window-next` &lt;C-N> opens the window to the right of the current one
- `:window-prev` &lt;C-P> opens the window to the left of the current one
- `:! {cmd}` execute {cmd} on the system shell. See chapter "Using an external
  tag editor" for an example.
- `:seek {seconds}` jumps to the given position in the current song
- `:remove` d removes the song under the cursor from the playlist
- `:paste` p adds the last deleted song after the selected song in the playlist
- `:add` a appends a song or directory to the end of playlist
- `:default-action` &lt;CR> Depending on the item under the cursor, somthing
  different happens:
   - *Playlist* start playing the song under the cursor
   - *Library* and *SearchResult* append the song under the cursor to the
     playlist and start playing it
   - *Browser* on a song: append the song to the playlist and play it. On a
     directory: go down to that directory.
- `:insert` i inserts a song to the playlist. The song is inserted after the
  currently playing song.
- `:add-album` A adds all songs of the album of the selected song to the
  playlist
- `:move-up` &lt;Up> k moves the cursor one line up
- `:move-down` &lt;Down> j moves the cursor one line down
- `:move-in` l go down one level the directory hierarchy in the *Browser*
  window
- `:move-out` h go up one level in the directory hierarchy in the *Browser*
  window
- `:move-first` gg go to the first line in the current window
- `:move-last` G go to the last line in the current window
- `:scroll-up` &lt;C-Y> scrolls the contents of the current window up one line
- `:scroll-down` &lt;C-E> scrolls the contents of the current window down one
  line
- `:scroll-page-up` &lt;PageUp> &lt;C-B> scrolls the contents of the current
  window up one page
- `:scroll-page-down` &lt;PageDown> l&t;C-F> scrolls the contents of the
  current window down one page

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

You can add mappings with the `:map`-command.

    map q :quit<cr>

Have a look at the [default mappings]
(https://github.com/sol/vimus/blob/master/resource/default-mappings).


### Recipes

#### Using an external tag editor

`:!` can be used to invoke external programs, `%` is expanded to the current
path (you need to set the base path to your library for `%` to work).

    set-library-path /path/to/music/directory

    # invoke kid3-qt with current song on 'T'
    map T :!kid3-qt %<cr>

## Development

Join in at `#vimus` on freenode.

### Running the test suite

    cabal configure --enable-tests && cabal build && cabal test

### Issues with GHC 7.0.4 and earlier

If you use `ghci` for development you may be affected by [GHC bug #2615]
(http://hackage.haskell.org/trac/ghc/ticket/2615) (at least Ubuntu uses a
linker script for ncursesw).

The easiest way to get around this is [updating your GHC to the latest version]
(http://www.haskell.org/ghc/download).

### Resources

 * [libmpd documentation on Hackage](http://hackage.haskell.org/packages/archive/libmpd/latest/doc/html/Network-MPD.html)
 * [MPD protocol specification](http://www.musicpd.org/doc/protocol/)

[test-shouldbe]:  https://github.com/sol/test-shouldbe
[hspec-shouldbe]: https://github.com/sol/hspec-shouldbe
