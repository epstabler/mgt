# Haskell implementations of MG structure-defining functions

-- still unstable, under development --

I installed ghc with [ghcup](https://www.haskell.org/ghcup/), and then used
[ghci 9.4.8](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html)
to develop and test the code in this directory.
No exotic language features are used, so hopefully this code will
run with the ghcup-recommended ghci versions for some time.

To run some simple examples of binary merge, type:

```
> cd MgBin
> ghci --package multiset
ghci> :l MgBinTests
ghci> ex0019a
ghci> ex0019b
ghci> ex0204a
```

See the code and comments for many other examples.
