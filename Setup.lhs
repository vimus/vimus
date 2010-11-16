#!/usr/bin/env runhaskell
> import Distribution.Simple
> import System.Cmd
>
> main = defaultMainWithHooks hooks
>   where hooks = simpleUserHooks { runTests = runTests' }
>
> runTests' _ _ _ _ = system "tests/run.sh" >> return ()
