# Haskell implementations of MG structure-defining functions

I installed ghc with [ghcup](https://www.haskell.org/ghcup/), and then used
[ghci 9.4.8](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html)
to develop and test the code in this directory.
No exotic language features are used, so this should 
run with the ghcup-recommended ghci versions for some time.

To run a simple example of binary merge, type:

```
> cd MgBin
> ghci
ghci> :l MgBinTests
ghci> ex0019a
ghci> ex0019b
ghci> ex0204a
```

To use Mg with its unbounded branching,
you need the multiset library which can be loaded with this command:

```
> cd Mg
> stack ghci --package multiset
ghci> :l MgTests
ghci> ex24a
ghci> ex0204a
ghci> ex1204a
```

See the code and comments for many other examples.
