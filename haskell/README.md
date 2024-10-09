# Haskell implementations of MG structure-defining functions

-- still unstable, under development --

I installed ghc with [ghcup](https://www.haskell.org/ghcup/), and then used
[ghci 9.4.8](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html)
to develop and test the code in this directory.
No exotic language features are used, so hopefully this code will
run with the ghcup-recommended ghci versions for some time.

To run some simple examples of binary merge, type:

```
> ghci --package multiset
ghci> :l MgBinTests
ghci> fig1a
ghci> fig1aO
ghci> fig2a
ghci> fig2aEll
ghci> fig2aOH
ghci> fig2bEll
ghci> fig1xOH
ghci> fig4OH
```

See the code and comments for many other examples.
