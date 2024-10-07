# Python implementations of MG structure-defining functions

Unlike Haskell, Python cannot have lexical items in a set if those items
are built with lists of features. (Python lists are not hashable.) So we use tuples.

Python also does not allow multisets of multisets,
so an alternative implementation of multisets is provided here,
based on frozendicts.
The frozendict module can be installed with ```pip install frozendict```.
That module is used in the definition of the class of SO objects.

Lexical items are (SO, label) pairs, where label is also a pair
(posFeatures, negFeatures), where both positive and negative features are in tuples.

Python tuples are written with parentheses.
Since parentheses are also used for grouping, 
an extra comma must be added to length 1 tuples for disambiguation.
The empty sequence is ().

The file ```mgTests.py``` has functions to print  our
data structures in more readable form:

```
> python
>>> from mgTests import *

>>> for i in g112: print(i)

((), (('V',), ('C',)))
((), (('V', 'Wh'), ('C',)))
(('Jo',), ((), ('D',)))
(('the',), (('N',), ('D',)))
(('which',), (('N',), ('D', 'Wh')))
(('who',), ((), ('D', 'Wh')))
(('cat',), ((), ('N',)))
(('dog',), ((), ('N',)))
(('food',), ((), ('N',)))
(('likes',), (('D', 'D'), ('V',)))
(('knows',), (('C', 'D'), ('V',)))

>>> ppMg(g112)

(, V -o C)
(, V.Wh -o C)
(Jo, D)
(the, N -o D)
(which, N -o D.Wh)
(who, D.Wh)
(cat, N)
(dog, N)
(food, N)
(likes, D.D -o V)
(knows, C.D -o V)

>>> 
```

The basic types of objects -- LI, SO, Label, WS -- are all defined in ```mgTypes.py```.
But once appropriate Python object classes are defined,
the python definitions of MG functions are similar to the Haskell.

This code is tested with [python 3.11](https://www.python.org/).
No exotic language features are used, so this should work for some time.

And for graphical structure display, I use nltk -- but that is optional.

```
> pip3 install nltk
> pip3 install frozendict
```

To run some simple examples, type:

```
> python3
>>> from mgTests import *
>>> ex24a()
>>> ex0019()
>>> ex0019a()
>>> ex24a()
>>> ex0204a()
>>> ex1204a()
```

See the code and comments for many other examples.
