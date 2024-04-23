""" mgBinL.py (since written in a functional style, annotated for mypy) """
from mgBin import *

def negWS(ws) -> bool:
  return [lso for lso in fset.toList(ws) if lso[1][0] != ()] != []

def ell(so) -> frozenset:
  """ map so to its derived workspace """
  if isinstance(so,tuple) and len(so) == 2 and \
     isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ): # lex
    return fset.fromList([(so,so[1])])
  elif isinstance(so,frozenset): # set
    ([nws],[pws]) = partition(negWS, [ell(s) for s in fset.toList(so)])
    (nlsos,others) = partition( lambda lso: lso[1][0] != (), fset.toList(nws) )
    if len(nlsos) != 1:
      raise RuntimeError("ell: zero or >1 neg LSOs in workspaces")
    (nso,(nfs,pfs)) = nlsos[0]
    imatches = [lso for lso in others if lso[1][1][0] == nfs[0]]
    if len(imatches)==1:
      (pso,plabel) = imatches[0]
      if maxx(fset.toList(pws))[0] != pso:
        raise RuntimeError("move-over-merge error")
      else: # im
        return d([nws])
    else: # em
      return d([nws,pws])
  else:
    raise TypeError("ell type error")
