""" fset.py  (f is for "frozen", i.e. immutable)
Since python sets cannot contain sets, we use frozensets.
>>> x = set([1,2])
>>> y = set([3,4])
>>> z = set([x,y])
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unhashable type: 'set'

>>> import fset
>>> x = fset.fromList([1,2])
>>> y = fset.fromList([3,4])
>>> z = fset.fromList([x,y])

since written in a functional style, annotated for mypy.
"""

def fromList(lst:list) -> frozenset:
    """ given list, return fset (i.e. frozenset) of its elements """
    return frozenset(lst)

def toList(fs:frozenset) -> list:
    """ given fset (i.e. frozenset), return its elements in a list """
    return list(fs)

def partition(f,fs:frozenset) -> tuple:
    """ given boolean function f and fset (i.e. frozenset), return (f-elements,non-f-elements) """
    yes, no = [], []
    for d in fs:
        if f(d): yes.append(d)
        else: no.append(d)
    return (frozenset(yes), frozenset(no))
