from mg import *  # this imports frozendict, mgTypes, mg

def ell(so):
  """ Map so to its derived workspace, if any. """
  if isinstance(so,LI) or isinstance(so,O):
    return so.to_ws()
  else:
    (negwss, poswss) = partition (lambda x: x.is_neg(), map(ell,so.to_tuple())) ## partition neg WSs (def in mgTypes.py)
    return d( negwss +  poswss )
