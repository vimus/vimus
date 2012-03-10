# Customizing  vimus

The file `$HOME/.vimusrc` is sourced on startup.  Lines starting with `#` are
comments.

## Using custom colors

You can use the `:color` command to customize colors.  For a light-on-dark
color scheme put the following into your `.vimusrc`.

    # a light-on-dark color scheme
    color tab green black
    color main cyan black
    color songstatus blue black
    color playstatus blue black
    color status red black
    color input white black

## Add custom mappings

You can add mappings with the `:map`-command.

    map q :quit<cr>

## Recipes

### Using an external tag editor

`:!` can be used to invoke external programs, `%` is expanded to the current
path (you need to set the base path to your library for `%` to work).

    set-library-path /path/to/music/directory

    # invoke kid3-qt on current song on 'T'
    map T :!kid3-qt %<cr>

# Development

Join in at `#vimus` on freenode.

## Running the test suite
The test suite depends on [test-shouldbe][] and [hspec-shouldbe][].

## Resources

 * [libmpd documentation on Hackage](http://hackage.haskell.org/packages/archive/libmpd/latest/doc/html/Network-MPD.html)
 * [MPD protocol specification](http://www.musicpd.org/doc/protocol/)

[test-shouldbe]:  https://github.com/sol/test-shouldbe
[hspec-shouldbe]: https://github.com/sol/hspec-shouldbe
