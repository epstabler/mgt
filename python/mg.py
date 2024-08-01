""" https://github.com/epstabler/mgt/tree/main/python/mg.py """
from mgTypes import *     # defines classes of objects: LI, SO, Label, WS
from typing import Tuple  # for type-checking with mypy

def mrg(seq: list) -> SO:
  """ merge: form multiset from sequence of SOs """
  return SO(seq)

def ck(labels: list) -> list: 
  """ remove first features from labels """
  return [f.ck() for f in labels[0:2]] + [Label((),()) for f in labels[2:]]

def t(ws: WS) -> WS:
  """ remove SOs with no features in their label """
  return ws.pfilter(lambda x: not(x[1].is_empty()))

def fplus(feature) -> Tuple[str,bool]:
  """ parse feature """
  if feature[-1] == '+': return (feature[:-1], True)
  else: return (feature, False)

def match(wss:list) -> Tuple[WS,WS]:
  """ partition elements of elements of WSs into (matchingWS, non-matchingWS) """
  (negwss, poswss) = partition (lambda x: x.is_neg(), wss) ## partition neg WSs (def in mgTypes.py)
  if len(negwss) != 1: raise RuntimeError("too many neg workspaces")
  so0, sos0, label0, labels0 = negwss[0]._sos[0], negwss[0]._sos[1:], negwss[0]._labels[0], negwss[0]._labels[1:]
  (f, plus) = fplus(label0._neg[0])
  (IMmatches, IMothers) = WS(sos0,labels0).ppartition(lambda x: x[1]._pos[0] == f) # partition matches
  if IMmatches._sos:
    so1, label1 = IMmatches._sos[0], IMmatches._labels[0]
    if len(poswss) != 1 or str(poswss[0]._sos[0]) != str(so1): raise RuntimeError("move-over-merge error")
    return ( WS([so0,so1], [label0, label1]), IMothers )
  else:
    pws, pwss = poswss[0], poswss[1:]
    (EMmatches, EMothers) = pws.ppartition(lambda x: x[1]._pos[0] == f) # partition matches
    if EMmatches._sos:
      so1, label1 = EMmatches._sos[0], EMmatches._labels[0]
      if plus and str(IMothers) == str(EMothers):                       # str for frozen dict comparison issues
        moreComps = atb(label1, EMothers, pwss)
        return ( WS([so0,so1], [label0, label1]).pappend(moreComps), IMothers )
      else:
        if pwss != []: raise RuntimeError("match: too many em pos workspaces")
        return ( WS([so0,so1], [label0, label1]), IMothers.pappend(EMothers) )
    else: raise RuntimeError("match: no matching pos workspaces")

def atb(label, movers, wss) -> WS:
  additionalComplementWS = WS([],[])
  for ws in wss:
    (matches, others) = ws.ppartition(lambda x: str(x[1]) == str(label)) # str for comparison issues
    if len(matches._sos) == 1: # and others == movers:
      additionalComplementWS = additionalComplementWS.pappend(matches)
    else:
      raise RuntimeError("atb: non-matching pos workspaces")
  return additionalComplementWS

def smc(ws: WS) -> WS:
  """ if WS has no 2 pos labels with same 1st feature, return WS """
  sofar = []
  for label in ws._labels:
    if label.is_pos():
      if label._pos[0] in sofar: raise RuntimeError('smc violation blocked')
      else: sofar.append(label._pos[0])
  return ws

def d(wss:list) -> WS:
  """ the derivational step: given list of workspaces, return derived workspace """
  (matches, others) = match(wss)
  return smc( t( WS( [mrg(matches._sos)] + matches._sos[1:], ck(matches._labels) ) ).pappend(others) )
