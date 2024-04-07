# Haskell implementations of MG structure-defining functions

I installed ghc with [ghcup](https://www.haskell.org/ghcup/), and then used
[ghci 9.4.8](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html)
to develop and test the code in this directory.
No exotic language features are used, so hopefully this code will
run with the ghcup-recommended ghci versions for some time.

To run a simple example of binary merge, type:

```
> ghci
ghci> :load MgBinTests
ghci> ex07c
```

To use Mg with its unbounded branching,
you need the multiset library which can be loaded with this command:

```
> stack ghci --package multiset
ghci> :load MgTests
ghci> ex07c
ghci> ex22c
```

See the code and comments for many other examples.
