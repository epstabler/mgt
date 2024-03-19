""" mgBin.py (since written in a functional style, annotated for mypy) """
import fset
from listfs import * # for partition, unzip

def mrg(lst: list) -> frozenset:
  """ merge """
  return fset.fromList(lst)

def ck(labels: list) -> list: 
  """ remove first features from already matched labels[0] and labels[1] """
  [(nns,nps), ((),pps)] = labels
  return [(nns[1:],nps), ((),pps[1:])]

def t(lsos:list) -> list:
  """ remove LSOs with no features in their label """
  return [x for x in lsos if x[1][1]]

def soSize(so) -> int:
  """ calculate the size of a syntactic object """
  if isinstance(so,tuple) and len(so) == 2 and \
     isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ): # lex
    return 1
  elif isinstance(so,frozenset): # fset
    return 1 + sum(map(soSize, fset.toList(so)))
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

def smc(lsos: list) -> frozenset:
  """ if LSOs have no pos feature in common, return them """
  firstPosFeatures = [lso[1][1][0] for lso in lsos if lso[1][0] == ()]
  if len(firstPosFeatures) < len(set(firstPosFeatures)):
    return fset.fromList([])
  else:
    return fset.fromList(lsos)

def match(wss:list) -> tuple:
  """ given list of workspaces, return ([head,complement],otherLSOs) """
  (ws0,wss0) = (wss[0],wss[1:])
  (heads, others) = partition(lambda lso: lso[1][0] != (), fset.toList(ws0))
  if not(len(heads)==1):
    raise RuntimeError('match: 0 or >1 neg lsos in ws0')
  h = heads[0]
  f = h[1][0][0]
  (ics,iothers) = partition(lambda lso: lso[1][1][0] == f, others)
  if ics and wss0 == []:    # im
    return ([h,ics[0]], iothers)
  elif ics==[] and len(wss0)==1:   # em
    ws1 = wss0[0]
    lsos = fset.toList(ws1)
    c = maxx(lsos)
    if c[1][0] == () and c[1][1][0] == f:
      others1 = [x for x in lsos if x != c]
      return ([h,c], others + others1)
    else: RuntimeError('match: complement')
  else:
    raise RuntimeError('match')

def d(wss:list) -> frozenset:
  """ the derivational step: given list of workspaces, return derived workspace """
  (matched, movers) = match(wss)
  (sos,labels) = unzip(matched)
  return smc( t( list(zip( [mrg(sos)]+sos[1:], ck(labels))) + movers))
