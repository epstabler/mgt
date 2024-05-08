""" mgTests.py """
import frozendict
from mgO import *
#from nltk.tree import Tree # optional -- for graphical display

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
  elif isinstance(so,frozendict.frozendict): # set
    sos = fmultiset.toList(so)
    s = '{ '
    for j,x in enumerate(sos):
      s += so2str(i+2, x)
      if j < len(sos)-1:
        s += "\n" + (i * ' ')
    s += ' }'
    return s
  else: # phTree
    return ph2str(i,so)

def so2str0(n, so): # derivation t, a nested list, as pretty string
    if isinstance(so,tuple) and len(so) == 2 and \
         isinstance(so[0],tuple) and ( len(so[0]) == 0 or isinstance(so[0][0],str) ): # lex
        lexStr = '(%s,%s)' % (" ".join(so[0]), label2str(so[1]))
        return '%s%s' % (n*' ', lexStr)
    elif isinstance(so,frozendict.frozendict): # set
        sos = fmultiset.toList(so)
        if isinstance(sos[0], list):
            s = '%s{\n%s' % ((n*' '),so2str(n, sos[0]))
            for element in sos[1:]: s += ',\n%s' % so2str(n+2, element)
        else:
            s = '%s{ %s' % (n*' ', so2str(n, sos[0]))
            for element in sos[1:]: s += ',\n%s' % so2str(n+2, element)
        return(s + ' }')
    elif (isinstance(so, list) or isinstance(so, tuple)) and len(so)>0:
        if isinstance(so[0], list):
            s = '%s[\n%s' % ((n*' '), so2str(n, sos[0]))
            for element in sos[1:]: s += ',\n%s' % so2str(n+2, element)
        else:
            s = '%s[%s' % (n*' ', so2str(n, so[0]))
            for element in sos[1:]: s += ',\n%s' % so2str(n+2, element)
        return(s + ']')
    else:
        raise TypeError('so2str type error')

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

def ph2nltk(ph):
  """ convert phTree to nltk Tree format -- Tree must be imported from nltk.tree """
  if isinstance(ph,tuple) and len(ph) == 2 and \
     isinstance(ph[0],tuple) and ( len(ph[0]) == 0 or isinstance(ph[0][0],str) ): # lex
    leaf = '(%s,%s)' % (" ".join(ph[0]), label2str(ph[1]))
    return Tree(leaf,[])
  else:
    return Tree('*', list(map(ph2nltk, ph)))

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
    if not(isinstance(ws,frozendict.frozendict)):
        raise TypeError('ws2str type error')
    else: # set
        lsos = fmultiset.toList(ws)
        if isinstance(lsos[0], list):
            s = '%s{\n%s' % ((n*' '),lso2str(n, lsos[0]))
            for element in lsos[1:]: s += ',\n%s' % lso2str(n+2, element)
        else:
            s = '%s{\n%s' % (n*' ', lso2str(n, lsos[0]))
            for element in lsos[1:]: s += ',\n%s' % lso2str(n+2, element)
        return(s + '\n}')

# create lexical LSO
def lexLSO(x):
    return (x, x[1])

# create lexical WS
def lexWS(x):
    return fmultiset.fromList([lexLSO(x)])

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

def ex00():
    ppMg(g112)

def exA():
    return fmultiset.fromList([
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
  return fmultiset.fromList([
       (('Jo',), ((),('D',))),
       exA() ])

def exBa():
  ppSO(exB())

def exC():
  return fmultiset.fromList([
      ((), (('V','Wh'),('C',))),
      exB() ])

def exCa():
  ppSO(exC())

def ex00a():
  return fmultiset.fromList([
            (('likes',),(('D','D'),('V',))),
            (('who',), ((),('D','Wh'))) ])

def ex00b():
  ppWS(ell(ex01()))

def ex01():
  return fmultiset.fromList(
    [ ((), (('V','Wh'),('C',))),
      fmultiset.fromList(
        [ (('Jo',), ((),('D',))),
          fmultiset.fromList([
            (('likes',),(('D','D'),('V',))),
            (('who',), ((),('D','Wh'))) ]) ]) ])

def ex01a():
  ppSO(ex01())

def ex01b():
  ppWS(ell(ex01()))

def ex02():
  return fmultiset.fromList([
    (('which',), (('N',),('D','Wh'))),
    (('food',), ((),('N',))) ])

def ex02b():
  ppWS(ell(ex02()))

def ex03():
  return fmultiset.fromList([
    (('Jo',), ((),('D',))),
      #fmultiset.fromList([
      #  (('the',), (('N',),('D',))),
      #  (('cat',), ((),('N',))) ]),
    fmultiset.fromList([
        (('likes',),(('D','D'),('V',))),
        ex02() ]) ])

def ex03b():
  ppWS(ell(ex03()))

def ex04():
  return fmultiset.fromList([
    ((), (('V','Wh'),('C',))),
    fmultiset.fromList([
      fmultiset.fromList([
        (('the',), (('N',),('D',))),
        (('cat',), ((),('N',))) ]),
      fmultiset.fromList([
        (('likes',),(('D','D'),('V',))),
        ex02() ]) ]) ])

def ex04b():
  ws = ell(ex04())
  ppWS(ws)
  print('derived structure has size %d' % len(ws))

def ex05():
  return fmultiset.fromList([
   fmultiset.fromList([
    ((), (('V','Wh'),('C',))),
    fmultiset.fromList([
      fmultiset.fromList([
        (('the',), (('N',),('D',))),
        (('cat',), ((),('N',))) ]),
      fmultiset.fromList([
        (('likes',),(('D','D'),('V',))),
        ex02() ]) ]) ]),
    ex02()])

def ex05b():
  ws = ell(ex05())
  ppWS(ws)
  print('derived ws has %d lso(s)' % len(ws))

def ex06():
  return fmultiset.fromList([
    (('knows',), (('C','D'),('V',))),
    ex05() ])

def ex06b():
  ws = ell(ex06())
  ppWS(ws)
  print('derived ws has %d lso(s)' % len(ws))

# This is the example in Figure 1
def ex07():
  return fmultiset.fromList([
    ((), (('V',),('C',))),
    fmultiset.fromList([
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

# Example of Figure 2, demonstrating multiple occurrences
def ex08():
  return fmultiset.fromList([
        ((), (('T',),('C',))),
        fmultiset.fromList([
          (('the man',), ((),('D', 'K', 'Scr'))),
          fmultiset.fromList([
            (('the man',), ((),('D', 'K', 'Scr'))),
             fmultiset.fromList([
               ((), (('v', 'K', 'Scr'),('T',))),
               fmultiset.fromList([
                 (('the man',), ((),('D', 'K', 'Scr'))),
                 fmultiset.fromList([
                   (('carefully',), (('v','Scr'), ('v',))),
                   fmultiset.fromList([
                     (('the man',), ((),('D', 'K', 'Scr'))),
                     fmultiset.fromList([
                       (('the man',), ((),('D', 'K', 'Scr'))),
                       fmultiset.fromList([
                         ((), (('V','K','D'), ('v',))),
                         fmultiset.fromList([
                           (('praises',), (('D',), ('V',))),
                           (('the man',), ((),('D', 'K', 'Scr'))) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

def ex08a():
  ppSO(ex08())

def ex08b():
  ws = ell(ex08())
  ppWS(ws)
  print('derived ws has %d lso(s)' % len(ws))

def ex08c():
  ppSO(ord_svo(ex08()))

def ex08d():
  ppSO(ord_sov(ex08()))

# example grammar from section 1.1.2
g121 = [
    ((), (("T",), ("C,"))),
    ((), (("V",), ("T",))),
    ((), (("Pred", "D"), ("Predx",))),
    (("is",), (("A",), ("V",))),
    (("cuma",), (("Predx",), ("A",))),
    (("e",), ((), ("D",))),
    (("na",), (("D",), ("Pred",))),
    (("shamhradh",), ((), ("D",))),
    (("fhomhar",), ((), ("D",))),
    (("gheimhread",), ((), ("D",))),
    (("no",), (("Pred", "Pred+"), ("Pred",)))
    ]

def ex09():
    ppMg(g121)

def ex20():
  return fmultiset.fromList([
    (("na",), (("D",), ("Pred",))),
    (("gheimhread",), ((), ("D",))) ])

def ex21():
  return fmultiset.fromList([
    (("no",), (("Pred", "Pred+"), ("Pred",))),
    ex20() ])

def ex22():
  return fmultiset.fromList([
    fmultiset.fromList([
      (("na",), (("D",), ("Pred",))),
      (("shamhradh",), ((), ("D",))) ]),
    fmultiset.fromList([
      (("na",), (("D",), ("Pred",))),
      (("fhomhar",), ((), ("D",))) ]),
    ex21() ])

def ex22so():
  ppSO(ex22())

def ex22a():
  ppWS(ell(ex22()))

def ex22b():
  ppSO(ord_svo(ex22()))

def ex22c():
  ppSO(ord_sov(ex22()))

# This is example (10) of Figure 3 but with identical coordinates
def ex23():
  return fmultiset.fromList([
    fmultiset.fromList([
      (("na",), (("D",), ("Pred",))),
      (("gheimhread",), ((), ("D",))) ]),
    fmultiset.fromList([
      (("na",), (("D",), ("Pred",))),
      (("gheimhread",), ((), ("D",))) ]),
    ex21() ])

def ex23so():
  ppSO(ex23())

def ex23a():
  ppWS(ell(ex23()))

def ex23b():
  ppSO(ord_svo(ex23()))

def ex23c():
  ppSO(ord_sov(ex23()))

# this example is from Figure 4
def ex24():
  return fmultiset.fromList([
    ((), (("V",),("C",))),
      fmultiset.fromList([
        (("Jo",), ((),("D",))),
        fmultiset.fromList([
          (("likes",), (("D", "D"), ("V",))),
          fmultiset.fromList([
            (("blueberries",), ((), ("D",))),
            (("bayberries",), ((), ("D",))),
            (("raspberries",), ((), ("D",))),
            (("mulberries",), ((), ("D",))),
            fmultiset.fromList([
              (("and",), (("D","D+"),("D",))),
              (("brambleberries",), ((), ("D",))) ]) ]) ]) ]) ])

def ex24so():
  ppSO(ex24())

def ex24a():
  ppWS(ell(ex24()))

def ex24b():
  ppSO(ord_svo(ex24()))

def ex24c():
  ppSO(ord_sov(ex24()))

# this example is (an English approximation to) Figure 5, left
def ex25():
  return fmultiset.fromList([
    (("who",), ((), ("D","Wh"))),
    fmultiset.fromList([
           ((), (("V","Wh"),("C",))),
           fmultiset.fromList([
             fmultiset.fromList([
               (("Maria",), ((), ("D",))),
               fmultiset.fromList([
                 (("likes",), (("D","D"),("V",))),
                 (("who",), ((), ("D","Wh"))) ]) ]),
             fmultiset.fromList([
               (("and",), (("V","V+"),("V",))),
               fmultiset.fromList([
                 (("Ewa",), ((), ("D",))),
                 fmultiset.fromList([
                   (("hates",), (("D","D+"),("V",))),
                   (("who",), ((), ("D","Wh"))) ]) ]) ]) ]) ]) ])

def ex25so():
  ppSO(ex25())

def ex25a():
  ppWS(ell(ex25()))

def ex25b():
  ppSO(ord_svo(ex25()))

def ex25c():
  ppSO(ord_sov(ex25()))

# we can have ATB with any number of coordinates, extending the previous example
def ex26():
  return fmultiset.fromList([
         (("who",), ((), ("D","Wh"))),
         fmultiset.fromList([
           ((), (("V","Wh"),("C",))),
           fmultiset.fromList([
             fmultiset.fromList([
               (("Maria",), ((), ("D",))),
               fmultiset.fromList([
                 (("likes",), (("D","D+"),("V",))),
                 (("who",), ((), ("D","Wh"))) ]) ]),
             fmultiset.fromList([
               (("Max",), ((), ("D",))),
               fmultiset.fromList([
                 (("tolerates",), (("D","D+"),("V",))),
                 (("who",), ((), ("D","Wh"))) ]) ]),
             fmultiset.fromList([
               (("Zuzanna",), ((), ("D",))),
               fmultiset.fromList([
                 (("pities",), (("D","D+"),("V",))),
                 (("who",), ((), ("D","Wh"))) ]) ]),
             fmultiset.fromList([
               (("and",), (("V","V+"),("V",))),
               fmultiset.fromList([
                 (("Ewa",), ((), ("D",))),
                 fmultiset.fromList([
                   (("hates",), (("D","D+"),("V",))),
                   (("who",), ((), ("D","Wh"))) ]) ]) ]) ]) ]) ])

def ex26so():
  ppSO(ex26())

def ex26a():
  ppWS(ell(ex26()))

def ex26b():
  ppSO(ord_svo(ex26()))

def ex26bntlk():
  ph2nltk(ord_svo(ex26())).draw()

def ex26c():
  ppSO(ord_sov(ex26()))

# example from \S1.3.3 of the paper: replicating Stabler (2001: \S2.1)
#   but without representing the strings of expressions as triples,
#   and with a deterministic free-affix transduction instead of many new derivational rules
def g133(): return [
    ((), (("T",),("C",))),
    ((), (("T","Wh"),("C",))),

    (("-s",), (("Modal","K"),("T",))),
    (("-s",), (("Have","K"),("T",))),
    (("-s",), (("Be","K"),("T",))),
    (("-s",), (("Vx","K"),("T",))),

    (("will",), (("Have",),("Modal",))),
    (("will",), (("Be",),("Modal",))),
    (("will",), (("Vx",),("Modal",))),

    (("have",), (("Been",),("Have",))),
    (("have",), (("Ven",),("Have",))),

    (("be",), (("Ving",),("Be",))),
    (("been",), (("Ving",),("Been",))),

    ((), (("V","D"),("Vx",))),
    (("-en",), (("V","D"),("Ven",))),
    (("-ing",), (("V","D"),("Ving",))),

    (("eat",), (("D","K"),("V",))),
    (("laugh",), ((),("V",))),

    (("the",), (("N",),("D","K"))),
    (("which",), (("N",),("D","K","Wh"))),

    (("king",), ((),("N",))),
    (("pie",), ((),("N",)))
    ]

def ex27a():
  ppMg(g133())

def ex27f():
  return fmultiset.fromList([
          fmultiset.fromList([
            (("which",), (("N",),("D","K","Wh"))),
            (("pie",), ((), ("N",))) ]),
          fmultiset.fromList([
            ((), (("T","Wh"),("C",))),
            fmultiset.fromList([
              fmultiset.fromList([
                (("the",), (("N",),("D","K"))),
                (("king",), ((), ("N",))) ]),
              fmultiset.fromList([
                (("-s",), (("Have","K"),("T",))),
                fmultiset.fromList([
                  (("have",), (("Been",),("Have",))),
                  fmultiset.fromList([
                    (("been",), (("Ving",),("Been",))),
                    fmultiset.fromList([
                      fmultiset.fromList([
                        (("the",), (("N",),("D","K"))),
                        (("king",), ((), ("N",))) ]),
                      fmultiset.fromList([
                        (("-ing",), (("V","D"),("Ving",))),
                        fmultiset.fromList([
                          fmultiset.fromList([
                             (("which",), (("N",),("D","K","Wh"))),
                             (("pie",), ((), ("N",))) ]),
                          fmultiset.fromList([
                            (("eat",), (("D","K"),("V",))),
                            fmultiset.fromList([
                               (("which",), (("N",),("D","K","Wh"))),
                               (("pie",), ((), ("N",))) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

def ex27g():
  ppWS(ell(ex27f()))

def ex27h():
  ppSO(ord_svo(ex27f()))

def ex27i():
  ppSO(ord_sov(ex27f()))

def ex27hntlk():
  ph2nltk(ord_svo(ex27f())).draw()

#ex08d()
#ex25b()
#ex27hntlk()
ex27h()
