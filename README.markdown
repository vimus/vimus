# Customizing  vimus

The file `$HOME/.vimusrc` is sourced on startup.

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

# Development

Join in at `#vimus` on freenode.

## Running the test suite
The test suite depends on [test-shouldbe][] and [hspec-shouldbe][].

[test-shouldbe]:  https://github.com/sol/test-shouldbe
[hspec-shouldbe]: https://github.com/sol/hspec-shouldbe
