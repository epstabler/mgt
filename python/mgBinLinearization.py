""" mgBinLinearization.py (since written in a functional style, annotated for mypy) """
from mgBinTransduction import *

def ord_svo(so) -> tuple:
  """ maps so to ordered phTree """
  if isinstance(so,tuple) and len(so) == 2 and \
     isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ): # lex
    return so
  else:
    (nso, pso, _, posfs) = ord(so)
    onso,opso = ord_svo(nso), ord_svo(pso)
    if isinstance(onso,tuple) and len(onso) == 2 and \
       isinstance(onso[0],tuple) and ( len(onso[0]) == 0 or isinstance(onso[0][0],str) ): # lex
      if len(posfs) > 1: return (onso,)
      else: return (onso, opso)
    else:
      if len(posfs) > 1: return (onso,)
      else: return (opso, onso)

def ord_sov(so) -> tuple:
  """ maps so to ordered phTree """
  if isinstance(so,tuple) and len(so) == 2 and \
     isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ): # lex
    return so
  else:
    (nso, pso, _, posfs) = ord(so)
    onso,opso = ord_sov(nso), ord_sov(pso)
    if len(posfs) > 1: return (onso,)
    else: return (opso, onso)

def ord(so: frozenset) -> tuple:
  """ maps so to (head, comp, head_pos_features, comp_positive-features) """
  if not(isinstance(so,frozenset)): raise TypeError("ord type error")
  ([nws],[pws]) = partition(negWS, [ell(s) for s in fset.toList(so)])  # NB! inefficient
  (nlsos,others) = partition( lambda lso: lso[1][0] != (), fset.toList(nws) )
  (nso,(nfs,pfs)) = nlsos[0]
  imatches = [lso for lso in others if lso[1][1][0] == nfs[0]]
  if len(imatches)==1: # im
    (pso,([],posfs)) = imatches[0]
    return (nso, pso, pfs, posfs)
  else: # em
    (pso,([],posfs)) = maxx(fset.toList(pws))
    return (nso, pso, pfs, posfs)
