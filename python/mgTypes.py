""" https://github.com/epstabler/mgt/tree/main/python/mgTypes.py """
import frozendict

class SO:
  r""" lexical item or multiset (where multisets are frozendicts) """
  def __init__(self, spec):
    if not(isinstance(spec,LI) or isinstance(spec,list)):
      raise TypeError( "SO: init expected LI or list of SOs")
    else:
      if isinstance(spec,LI):
        self._so = spec
      elif isinstance(spec,list):
        """ given list, return multiset (i.e. frozendict) of those elements """
        counts = {}
        for e in spec:
          if not(isinstance(e,LI) or isinstance(e._so,frozendict.frozendict)):
            raise TypeError( "SO: Expected tuple of LIs and SOs")
          if not(e in counts.keys()): counts[e] = 1
          else: counts[e] += 1
        self._so = frozendict.frozendict(counts)

  def __repr__(self) -> str:
    #if isinstance(self._so,frozendict.frozendict):
    return repr(self._so)

  def str(self) -> str: return '{%s}' % ', '.join([e.str() for e in self.to_tuple()])

  def to_tuple(self) -> tuple: # given frozendict, return its elements in a tuple
    #if not(isinstance(self._so,frozendict.frozendict)):
    #  raise RuntimeError('SO.to_tuple() defined only when SO is a multiset')
    lst = []
    for y in self._so.keys():
      lst.extend(self._so[y] * [y])
    return tuple(lst)

  def pp(self): # print pretty string
    print(self.ppi(0))

  def ppi(self, i): # return pretty, indented string
    sos = self.to_tuple()
    s = '{ '
    for j,x in enumerate(sos):
      s += x.ppi(i+2)
      if j < len(sos)-1:
        s += "\n" + ((i+2) * ' ')
    s += ' }'
    return s

class Label:
  r"""  labels are conditionals (_neg,_pos) where _neg, _pos are tuples of features """
  def __init__(self, antecedent, consequent):
    if not(isinstance(antecedent,tuple)) or not(isinstance(consequent,tuple)):
      raise TypeError("Label: Expected antecedent and consequent tuples")
    if antecedent and not(isinstance(antecedent[0],str)):
      raise TypeError("Label: Expected antecedent to be tuple of str")
    if consequent and not(isinstance(consequent[0],str)):
      raise TypeError("Label: Expected consequent to be tuple of str")
    self._neg = antecedent
    self._pos = consequent

  def __repr__(self) -> str:
    return '(%s, %s)' % (self._neg, self._pos)

  def str(self) -> str:
    if len(self._neg) > 0:
      return '%s -o %s' % ('.'.join(self._neg), '.'.join(self._pos))
    elif len(self._pos) > 0:
      return '.'.join(self._pos)
    else:
      return 'T'

  def pair(self): return (self._neg, self._pos)

  def is_neg(self) -> bool: return len(self._neg) > 0
  
  def is_pos(self) -> bool: return len(self._neg) == 0

  def is_empty(self) -> bool: return len(self._neg) == 0 and len(self._pos) == 0

  def ck(self):
    if len(self._neg) > 0:
      return Label(self._neg[1:],self._pos)
    elif len(self._pos) > 0:
      return Label((),self._pos[1:])
    else:
      raise RuntimeError('ck() not defined on empty label')

class LI(SO):
  r"""  a lexical item is a pair: ([string],(features,features))  """
  def __init__(self, ph, l):
    if not( isinstance(ph,tuple) ):
      raise TypeError( "LI: Expected (ph,pair), where ph a tuple, got %s" % repr(ph) )
    if ph and not(isinstance(ph[0],str)):
      raise TypeError( "LI: Expected ph where each element of ph a string, got element %s" % repr(ph[0]) )
    elif not(isinstance(l,tuple)) or not(len(l)==2) or (l[0] and not(isinstance(l[0][0],str))):
      raise TypeError( "LI: Expected pair = (neg,pos), got %s" % str(type(l)))
    else:
      self._ph = ph
      self._label = Label(l[0],l[1])

  def __repr__(self) -> str:  return str((self._ph, self._label))

  def str(self) -> str:  return '(%s,%s)' % (' '.join(self._ph), self._label.str())

  def ppi(self, i): return self.str()

  def to_nltk(self):
    leaf = '(%s,%s)' % (" ".join(self._ph), self._label.str())
    return Tree(leaf,[])

  def to_ws(self): return WS([self],[self._label])

class O(SO):
  r""" Objects in this class are tuples (of tuples of ...).
       They can be regarded as linearly ordered trees with no labels on internal nodes
  """
  def __init__(self, t):
    if not(isinstance(t,tuple)):
      raise TypeError( "O: init expected tuple of SOs" )
    else:
      self._tuple = t

  def __repr__(self) -> str:
    return str(self._tuple)

  def str(self) -> str:
    return str(self._tuple)

  def pp(self): print(self.ppi(0))

  def ppi(self,i):
    if len(self._tuple) == 1: return "%s" % self._tuple[0].str()
    else:
      s = '( '
      for j,x in enumerate(self._tuple):
        s += x.ppi(i+2)
        if j < len(self._tuple)-1:
          s += "\n" + (i * ' ')
      s += ' )'
      return s

  def to_ws(self): return WS([self],[Label([],[])])

  def to_nltk(self):
    return Tree('*', [e.to_nltk() for e in ph])

class WS:
  r""" a workspace is a pair (SOs, Labels)  """
  def __init__(self, sos, labels):
    if not( isinstance(sos,list) and isinstance(labels,list) ):
      raise TypeError( "WS: Expected pair of lists: (sos,labels)")
    else:
      self._sos = sos
      self._labels = labels

  def __repr__(self) -> str:
    return '(%s, %s)' % (self._sos.__repr__(), self._labels.__repr__())

  def str(self) -> str:
    return '([%s], [%s])' % (', '.join([s.str() for s in self._sos]), ', '.join([s.str() for s in self._labels]))

  def pp(self): 
    for i in range(len(self._sos)):
      self._sos[i].pp()
      print(self._labels[i].str())
      print('--')

  def is_neg(self):
    for f in self._labels:
      if f.is_neg(): return True
    return False

  def pappend(self,ws2):
    a, b = ws2._sos, ws2._labels
    if len(a) != len(b):
      raise TypeError( "WS: pappending lists of different lengths")
    return WS(self._sos + a, self._labels + b)

  def pfilter(self,p):
    yes0, yes1 = [], []
    for i in range(len(self._sos)):
      if p((self._sos[i],self._labels[i])):
        yes0.append(self._sos[i])
        yes1.append(self._labels[i])
    return WS(yes0, yes1)

  def ppartition(self, p) -> tuple:
    yes0, yes1 = [], []
    no0, no1 = [], []
    for i in range(len(self._sos)):
      if p((self._sos[i],self._labels[i])):
        yes0.append(self._sos[i])
        yes1.append(self._labels[i])
      else:
        no0.append(self._sos[i])
        no1.append(self._labels[i])
    return (WS(yes0,yes1), WS(no0,no1))


# A FUNCTION ON LISTS
def partition(p, lst:list) -> tuple:
  """ given boolean function p, partition list into two lists: (p-elements, non-p elements) """
  yes, no = [], []
  for d in lst:
    if p(d): yes.append(d)
    else: no.append(d)
  return (yes, no)
