""" mgTransduction.py (not annotated for mypy, since mypy does not know frozendict) """
import frozendict
from mg import *

def negWS(ws:list) -> bool:
  return [lso for lso in fmultiset.toList(ws) if lso[1][0] != ()] != []

def ell(so):
  """ map so to its derived workspace """
  if isinstance(so,tuple) and len(so) == 2 and \
     isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ):
    return fmultiset.fromList([(so,so[1])])
  elif isinstance(so,frozendict.frozendict):
    ([nws],pwss) = partition(negWS, [ell(s) for s in fmultiset.toList(so)])
    pws = pwss[0]
    (nlsos,others) = partition( lambda lso: lso[1][0] != (), fmultiset.toList(nws) )
    if len(nlsos) != 1:
      raise RuntimeError("ell: zero or >1 neg LSOs in workspaces")
    (nso,(nfs,pfs)) = nlsos[0]
    imatches = [lso for lso in others if lso[1][1][0] == nfs[0]]
    if len(imatches)==1:
      (pso,plabel) = imatches[0]
      if maxx(fmultiset.toList(pws))[0] != pso or pwss[1:] != []:
        raise RuntimeError("move-over-merge error")
      else: # im
        return d([nws])
    else:  # em
      return d([nws] + pwss)
  else:
    raise TypeError("ell type error")
