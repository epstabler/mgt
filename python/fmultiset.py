""" fmultiset.py  (f is for "frozen", i.e. immutable)
Since python multisets cannot contain multisets, we use frozendicts.

NB: mypy does not know the datatype frozendict, so no type hints here.
"""
import frozendict

def fromList(lst):
    """ given list, return multiset (i.e. frozendict) of those elements """
    counts = {}
    for e in lst:
        if not(e in counts.keys()): counts[e] = 1
        else: counts[e] += 1
    return frozendict.frozendict(counts)

def toList(fms):
    """ given fmultiset (i.e. frozendict), return its elements in a list """
    lst = []
    for y in fms.keys():
        lst.extend(fms[y] * [y])
    return lst

def partition(f,fms):
    """ given boolean function f and fmultiset (i.e. frozendict), return (f-elements,non-f-elements) """
    yes, no = {}, {}
    for d in fms.keys():
        if f(d): yes[d] = fms[d]
        else: no[d] = fms[d]
    return (frozendict.frozendict(yes), fd.frozendict(no))
