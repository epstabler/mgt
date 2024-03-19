""" mg.py (not annotated for mypy, since mypy does not know frozendict) """
import frozendict, fmultiset
from listfs import * # for partition, unzip

def mrg(lst: list):
  """ merge """
  return fmultiset.fromList(lst)

def ck(labels: list) -> list:
  """ remove first features from already matched labels[0], labels[1] and T for rest """
  (nns,nps),((),pps) = labels[0],labels[1]
  return ((nns[1:],nps), ((),pps[1:])) + tuple([((),()) for label in labels[2:]])

def t(lsos:list) -> list:
  """ remove LSOs with no features in their label """
  return [x for x in lsos if x[1][1]]

def soSize(so) -> int:
  """ calculate the size of a syntactic object """
  if isinstance(so,tuple) and len(so) == 2 and \
     isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ): # lex
    return 1
  elif isinstance(so,frozendict.frozendict): # fmultiset
    return 1 + sum(map(soSize, fmultiset.toList(so)))
  else:  # phtree
    return 1 + sum(map(soSize, so))

def maxx(lsos: list) -> tuple:
  """ given LSOs (of a WS), return LSO with largest SO (the head) """
  sofar = 0
  maxLSO = ((()),((),()))
  for lso in lsos:
    lsoSize = soSize(lso[0])
    if lsoSize > sofar:
      sofar = lsoSize
      maxLSO = lso
  return maxLSO

def smc(lsos: list):
  """ if LSOs have no pos feature in common, return them """
  firstPosFeatures = [lso[1][1][0] for lso in lsos if lso[1][0] == ()]
  if len(firstPosFeatures) < len(set(firstPosFeatures)):
    return fmultiset.fromList([])
  else:
    return fmultiset.fromList(lsos)

def match(wss:list) -> tuple:
  """ given list of workspaces, return ([head,complement],otherLSOs) """
  (ws0,wss0) = (wss[0],wss[1:])
  (heads, others) = partition(lambda lso: lso[1][0] != (), fmultiset.toList(ws0))
  if not(len(heads)==1): raise RuntimeError("match: 0 or >1 neg lsos in ws0")
  h = heads[0]
  f = h[1][0][0]
  if f[-1] == '+':
    f, plus = f[:-1], True
  else:
    plus = False
  (ics,iothers) = partition(lambda lso: lso[1][1][0] == f, others)
  if ics and wss0 == []:    # im
    return ([h,ics[0]], iothers)
  elif ics==[] and len(wss0)>0:   # em
    ws1 = wss0[0]
    lsos = fmultiset.toList(ws1)
    c = maxx(lsos)
    if c[1][1][0] != f:
      raise RuntimeError("match: %r != %r" % (f,c[1][1][0]))
    else:
      others1 = [x for x in lsos if x != c]
      if plus and others == others1:
        return ([h,c]+atb(f, others, wss0[1:]), others)
      elif wss0[1:] == []:
        return ([h,c], others + others1)
      else:
        raise RuntimeError("match: too many wss")

def atb(f, movers, wss):
  """ collect comps with first feature f and others==movers """
  if wss == []:
    return []
  else:
    ws1 = wss[0]
    lsos = fmultiset.toList(ws1)
    c = maxx(lsos)
    if c[1][1][0] != f:
      raise RuntimeError("match: complement clash")
    else:
      others = [x for x in lsos if x != c]
      if others != movers:
        raise RuntimeError("match: mover clash")
      else:
        return [c] + atb(f, movers, wss[1:])

def d(wss:list):
  """ the derivational step: given list of workspaces, return derived workspace """
  (matched, movers) = match(wss)
  (sos,labels) = unzip(matched)
  return smc( t( list(zip( [mrg(sos)]+sos[1:], ck(labels))) + movers))
