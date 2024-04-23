""" mgO.py (not annotated for mypy, since mypy does not know frozendict) """
import frozendict
from mgL import *

def ord_svo(so) -> tuple:
  """ maps so to ordered phTree """
  if isinstance(so,tuple) and len(so) == 2 and \
     isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ): # so is lex
    return so
  else: # so is mset
    (nso, pso, _, posfs, pwss) = ord(so)
    nt, pt = ord_svo(nso), ord_svo(pso)
    plsos = list(map(maxx, pwss))
    psos = [x[0] for x in plsos]
    pts = map(ord_svo, psos)
    if isinstance(nt,tuple) and len(nt) == 2 and \
       isinstance(nt[0],tuple) and ( len(nt[0]) == 0 or isinstance(nt[0][0],str) ): # nt is lex
      if len(posfs) > 1: return (nt,)
      else: return (nt, pt) + tuple(pts)
    else: # nt is mset
      if len(posfs) > 1: return (nt,)
      else: return (pt,) + tuple(pts) + (nt,)

def ord_sov(so) -> tuple:
  """ maps so to ordered phTree """
  if isinstance(so,tuple) and len(so) == 2 and \
     isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ): # so is lex
    return so
  else: # so is mset
    (nso, pso, _, posfs, pwss) = ord(so)
    nt, pt = ord_sov(nso), ord_sov(pso)
    plsos = list(map(maxx, pwss))
    psos = [x[0] for x in plsos]
    pts = map(ord_sov, psos)
    if len(posfs) > 1: return (nt,)
    else: return (pt,) + tuple(pts) + (nt,)

def ord(so) -> tuple:
  """ maps so to (head, comp, head_pos_features, comp_positive-features, otherPosWSs) """
  if not(isinstance(so,frozendict.frozendict)): raise TypeError("ord type error")
  ([nws],pwss) = partition(negWS, [ell(s) for s in fmultiset.toList(so)])  # NB! inefficient
  (pws,pwss1) = (pwss[0],pwss[1:])
  (nlsos,others) = partition( lambda lso: lso[1][0] != (), fmultiset.toList(nws) )
  (nso,(fns,ps)) = nlsos[0]
  f = fns[0]
  imatches = [lso for lso in others if lso[1][1][0] == f]
  if len(imatches)==1: # im
    (pso,([],posfs)) = imatches[0]
    return (nso, pso, ps, posfs, pwss1)
  else: # em
    (pso,([],posfs)) = maxx(fmultiset.toList(pws))
    return (nso, pso, ps, posfs, pwss1)
