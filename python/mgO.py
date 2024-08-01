from mgH import *  # this imports frozendict, mgTypes, mg, mgL, mgH

def o_svo(so) -> O:
  """ map so to ordered svo tuple """
  if isinstance(so,LI):
    return O((so,))
  else:
    (nso, pso, _, posfs, pwss) = o(so)
    nt, pt = o_svo(nso), o_svo(pso)
    psos = [ws._sos[0] for ws in pwss]
    pts = tuple(map(o_svo, psos))
    if len(posfs) > 1:
      comps = silent((pt,) + pts)
    else:
      comps = (pt,) + pts
    if isinstance(nso,LI):
      return O( (nt,) + comps )
    else:
      return O( comps + (nt,) )

def o_sov(so) -> O:
  """ map so to ordered sov tuple """
  if isinstance(so,LI):
    return O((so,))
  else:
    (nso, pso, _, posfs, pwss) = o(so)
    nt, pt = o_sov(nso), o_sov(pso)
    psos = [ws._sos[0] for ws in pwss]
    pts = tuple(map(o_sov, psos))
    if len(posfs) > 1:
      comps = silent((pt,) + pts)
    else:
      comps = (pt,) + pts
    return O( comps + (nt,) )

def o(so) -> tuple:
  """ maps so to (head, comp, head_pos_features, comp_pos_features, otherPosWSs) """
  # NB: to get pos features of IM complement, we need to find them in ell(nws); otherwise, they're in pws
  if not(isinstance(so._so,frozendict.frozendict)): raise TypeError("o: type error")
  (negwss, poswss) = partition (lambda x: x.is_neg(), map(ell,so.to_tuple())) ## partition neg WSs (def in mgTypes.py)
  so0, sos0, label0, labels0 = negwss[0]._sos[0], negwss[0]._sos[1:], negwss[0]._labels[0], negwss[0]._labels[1:]
  (f, plus) = fplus(label0._neg[0])
  (IMmatches, IMothers) = WS(sos0,labels0).ppartition(lambda x: x[1]._pos[0] == f) # partition matches
  if IMmatches._sos:
    so1, label1 = IMmatches._sos[0], IMmatches._labels[0]
    return (so0, so1, label0._pos, label1._pos, poswss[1:])
  else:
    so1, label1 = poswss[0]._sos[0], poswss[0]._labels[0]
    return (so0, so1, label0._pos, label1._pos, poswss[1:])

def silent(x):
  if isinstance(x,tuple):
    return tuple([silent(o) for o in x])
  elif isinstance(x,O):
    if len(x._tuple) == 1: # lexical item
      ph, fs = x._tuple[0]._ph, x._tuple[0]._label.pair()
      newph = tuple(['('+w+')' for w in ph])
      return O((LI(newph, fs),))
    else:
      return O( tuple([silent(o) for o in x._tuple]) )
