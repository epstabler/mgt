""" listfs.py
       It is convenient to have these two list functions
       Since written in a functional style, annotated for mypy.
"""

def partition(f, lst:list) -> tuple:
    """ given boolean function f, partition list into two lists: (f-elements, non-f elements) """
    yes, no = [], []
    for d in lst:
        if f(d): yes.append(d)
        else: no.append(d)
    return (yes, no)

def unzip(pairs: list) -> tuple:
    """ we could use zip(*pairs), but mypi typing does not understand """
    firsts, seconds = [], []
    for (x,y) in pairs:
        firsts.append(x)
        seconds.append(y)
    return (firsts, seconds)
