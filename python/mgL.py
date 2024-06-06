""" https://github.com/epstabler/mgt/tree/main/python/mgL.py """
from mg import *  # this imports frozendict, mgTypes, mg

def ell(so):
  """ Map so to its derived workspace, if any. """
  if isinstance(so,LI) or isinstance(so,O): return so.to_ws()
  else:
    (negwss, poswss) = partition (lambda x: x.is_neg(), map(ell,so.to_tuple())) ## partition neg WSs (def in mgTypes.py)
    sos0, label0, labels0 = negwss[0]._sos[1:], negwss[0]._labels[0], negwss[0]._labels[1:]
    (f, plus) = fplus(label0._neg[0])
    (IMmatches, IMothers) = WS(sos0,labels0).ppartition(lambda x: x[1]._pos[0] == f) # partition matches
    if IMmatches._sos:
      if len(poswss) != 1 or \
         str(IMmatches._sos[0]) != str(poswss[0]._sos[0]): # str to avoid comparison issues
        raise RuntimeError("ell: move-over-merge error")
      return d( negwss )
    else:
      return d( negwss +  poswss )
