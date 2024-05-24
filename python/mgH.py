""" https://github.com/epstabler/mgt/tree/main/python/mgH.py """
from mgL import *           # this imports frozendict, mgTypes, mg, mgL

def inc(h) -> int:
  """ return the number of head-incorporator '+' at the beginning of h """
  if not(h) or not(isinstance(h[0],str)):
    raise RuntimeError("inc: expected tuple of strings")
  count = 0
  for i in range(len(h[0])):
    if h[0][i] == '+': count += 1
    else: break
  return count

def h(i, so):
  """ head movement, where i is the number of head-incorporators on governing head """
  if isinstance(so,LI):
    if i == 0: return ((), so)
    elif i == 1: return (so._ph, LI((), so._label.pair()))
    else: RuntimeError("h: incorporator requirements not met")
  else:
    (negwss, poswss) = partition (lambda x: x.is_neg(), map(ell,so.to_tuple())) ## partition neg WSs (def in mgTypes.py)
    so0 = negwss[0]._sos[0]
    pso0 = poswss[0]._sos[0]
    if isinstance(so0,LI):
      i0 = inc(so0._ph) + max([0,i-1])
      (hs,pso) = h(i0, pso0)
      psos = atbh(i0, hs, poswss[1:])
      if i == 0: return ((), SO([LI(hs + so0._ph, so0._label.pair()), pso] + psos))
      elif i == 1: return (hs + so0._ph, SO([LI((), so0._label.pair()), pso] + psos))
      else: return (so0._ph + hs, SO([LI((), so0._label.pair()), pso] + psos))
    else:
      (hs,so1) = h(i, so0)
      psos = [ws._sos[0] for ws in poswss]
      return (hs, SO([so1] + psos))

def atbh(i, hs, wss) -> list:
  """ collect additional comps with hs extracted, across-the-board """
  if wss == []: return []
  else:
    (hs0, pso) = h(i, wss[0]._sos[0])
    if hs0 != hs: raise RuntimeError("atbh: non-identical head")
    else: return [pso] + atbh(i, hs, wss[1:])
