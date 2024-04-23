import sys
from mgBinO import *

def tab(n):
  """ print n spaces """
  sys.stdout.write(n * ' ')

def label2str(label):
  """ convert label to pretty string """
  (negfs, posfs) = label
  if negfs == ():
    if posfs == (): return 'T'
    else: return '.'.join(posfs)
  else:
    return '.'.join(negfs) + ' -o ' + '.'.join(posfs)

def lex2str(pair):
  """ convert lexical item to pretty string """
  (words,label) = pair
  if words == []:
    return "([], %s)" % label2str(f)
  else:
    return '(%s, %s)' % (' '.join(words), label2str(label))

def ppMg(g):
  """ pretty print grammar """
  for i in g: print(lex2str(i))

def ppSO(so):
  """ pretty print SO """
  print(so2str(0,so))

def so2str(i,so):
  """ return pretty string representation of so """
  if isinstance(so,tuple) and len(so) == 2 and \
     isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ): # lex
    return '(%s,%s)' % (" ".join(so[0]), label2str(so[1]))
  elif isinstance(so,frozenset): # set
    sos = fset.toList(so)
    s = '{ '
    for j,x in enumerate(sos):
      s += so2str(i+2, x)
      if j < len(sos)-1:
        s += "\n" + (i * ' ')
    s += ' }'
    return s
  else:  # phTree
    return ph2str(i,so)

def ph2str(i,so):
  """ convert phTree to pretty string with indent i """
  if isinstance(so,tuple) and len(so) == 2 and \
     isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ): # lex
    return '(%s,%s)' % (" ".join(so[0]), label2str(so[1]))
  else:
    s = '[ '
    for j,x in enumerate(so):
      s += ph2str(i+2, x)
      if j < len(so)-1:
        s += "\n" + (i * ' ')
    s += ' ]'
    return s

def ppLSO(lso):
  """ pretty print labeled SO """
  print(lso2str(0,lso))

def lso2str(n, lso):
  (so,label) = lso
  return '( ' + so2str(0, so) + (', %s )' % label2str(label))

def ppWS(ws):
  """ pretty print workspace, i.e. set of LSOs """
  print(ws2str(0,ws))

def ws2str(n, ws):
  if not(isinstance(ws,frozenset)):
    raise TypeError('ws2str type error')
  else: # set
    lsos = fset.toList(ws)
    if isinstance(lsos[0], list):
      s = '%s{\n%s' % ((n*' '),lso2str(n, lsos[0]))
      for element in lsos[1:]: s += ',\n%s' % lso2str(n+2, element)
    else:
      s = '%s{\n%s' % (n*' ', lso2str(n, lsos[0]))
      for element in lsos[1:]: s += ',\n%s' % lso2str(n+2, element)
    return(s + '\n}')

# example grammar from section 1.1.2
g112 = [
  ((), (('V',),('C',))),
  ((), (('V','Wh'),('C',))),
  (('Jo',), ((),('D',))),
  (('the',), (('N',),('D',))),
  (('which',), (('N',),('D','Wh'))),
  (('who',), ((),('D','Wh'))),
  (('cat',), ((),('N',))),
  (('dog',), ((),('N',))),
  (('food',), ((),('N',))),
  (('likes',),(('D','D'),('V',))),
  (('knows',), (('C','D'),('V',)))
]

# create lexical LSO
def lexLSO(x):
  return (x, x[1])

# create lexical WS
def lexWS(x):
  return fset.fromList([lexLSO(x)])

def ex00():
  ppMg(g112)

def exA():
  return fset.fromList([
    (('likes',),(('D','D'),('V',))),
    (('who',), ((),('D','Wh'))) ])

def exAa():
  ppSO(exA())

def exAws():
  return d([lexWS((('likes',),(('D','D'),('V',)))),
            lexWS((('who',), ((),('D','Wh')))) ])

def exAb():
  ppWS(exAws())

def exAc():
  ppWS(ell(exA()))

def exB():
  return fset.fromList([
    (('Jo',), ((),('D',))),
    exA() ])

def exBa():
  ppSO(exB())

def exC():
  return fset.fromList([
    ((), (('V','Wh'),('C',))),
    exB() ])

def exCa():
  ppSO(exC())

def ex00a():
  return fset.fromList([
    (('likes',),(('D','D'),('V',))),
    (('who',), ((),('D','Wh'))) ])

def ex00b():
  ppWS(ell(ex01()))

def ex01():
  return fset.fromList(
    [ ((), (('V','Wh'),('C',))),
      fset.fromList(
        [ (('Jo',), ((),('D',))),
          fset.fromList([
            (('likes',),(('D','D'),('V',))),
            (('who',), ((),('D','Wh'))) ]) ]) ])

def ex01a():
  ppSO(ex01())

def ex01b():
  ppWS(ell(ex01()))

def ex02():
  return fset.fromList([
    (('which',), (('N',),('D','Wh'))),
    (('food',), ((),('N',))) ])

def ex02b():
  ppWS(ell(ex02()))

def ex03():
  return fset.fromList([
    (('Jo',), ((),('D',))),
    fset.fromList([
        (('likes',),(('D','D'),('V',))),
        ex02() ]) ])

def ex03b():
  ppWS(ell(ex03()))
  
def ex04():
  return fset.fromList([
    ((), (('V','Wh'),('C',))),
    fset.fromList([
      fset.fromList([
        (('the',), (('N',),('D',))),
        (('cat',), ((),('N',))) ]),
      fset.fromList([
        (('likes',),(('D','D'),('V',))),
        ex02() ]) ]) ])

def ex04b():
  ws = ell(ex04())
  ppWS(ws)
  print('derived structure has size %d' % len(ws))

def ex05():
  return fset.fromList([
   fset.fromList([
    ((), (('V','Wh'),('C',))),
    fset.fromList([
      fset.fromList([
        (('the',), (('N',),('D',))),
        (('cat',), ((),('N',))) ]),
      fset.fromList([
        (('likes',),(('D','D'),('V',))),
        ex02() ]) ]) ]),
    ex02()])

def ex05b():
  ws = ell(ex05())
  ppWS(ws)
  print('derived ws has %d lso(s)' % len(ws))

def ex06():
  return fset.fromList([
    (('knows',), (('C','D'),('V',))),
    ex05() ])

def ex06b():
  ws = ell(ex06())
  ppWS(ws)
  print('derived ws has %d lso(s)' % len(ws))

# This is the example in Figure 1
def ex07():
  return fset.fromList([
    ((), (('V',),('C',))),
    fset.fromList([
      (('Jo',), ((),('D',))),
      ex06() ]) ])

def ex07a():
  ppSO(ex07())

def ex07b():
  ws = ell(ex07())
  ppWS(ws)
  print('derived ws has %d lso(s)' % len(ws))

def ex07c():
  ppSO(ord_svo(ex07()))

def ex07d():
  ppSO(ord_sov(ex07()))

#ex07b()
ex07c()
