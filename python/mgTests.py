from mgO import *  # this imports frozendict, mgTypes, mg, mgL, mgH and mgO
from nltk.tree import Tree # optional -- for graphical display

def ppMg(g):         # pretty print grammar
  for i in g: print(i.str())

# example grammar from section 1.1.2
g112 = [
  LI((), (('V',),('C',))),               #0
  LI((), (('V','Wh'),('C',))),           #1
  LI(('Jo',), ((),('D',))),              #2
  LI(('the',), (('N',),('D',))),         #3
  LI(('which',), (('N',),('D','Wh'))),   #4
  LI(('who',), ((),('D','Wh'))),         #5
  LI(('cat',), ((),('N',))),             #6
  LI(('dog',), ((),('N',))),             #7
  LI(('food',), ((),('N',))),            #8
  LI(('likes',),(('D','D'),('V',))),     #9
  LI(('knows',), (('C','D'),('V',)))    #10
  ]

def ex00(): ppMg(g112)

def ex000(): return d([g112[3].to_ws(), g112[6].to_ws()]) # the cat
def ex001(): ex000().pp()

def ex002(): return d( [g112[4].to_ws(), g112[8].to_ws()] )  # which food
def ex003(): ex002().pp()

def ex004(): return d( [g112[9].to_ws(), ex002()] )  # likes which food
def ex005(): ex004().pp()

def ex006(): return d( [ex004(), ex000()] )  # the cat likes which food
def ex007(): ex006().pp()

def ex008(): return d( [ex006(), g112[1].to_ws()] )  # C[+wh] the cat likes which food
def ex009(): ex008().pp()

def ex0010(): return d( [ex008(), ex002()] )  # which food C[+wh] the cat likes which food
def ex0011(): ex0010().pp()

def ex0012(): return d( [g112[10].to_ws(), ex0010()] )  # knows which food C[+wh] the cat likes which food
def ex0013(): ex0012().pp()

def ex0014(): return d( [ex0012(), g112[2].to_ws()] )  # Jo knows which food C[+wh] the cat likes which food
def ex0015(): ex0014().pp()

## This is the example in Figure 1, with wh movement
def ex0016(): return d( [g112[0].to_ws(), ex0014()] )  # C Jo knows which food C[+wh] the cat likes which food
def ex0017(): ex0016().pp()
def ex0018(): ex0016()._sos[0].pp() # print head SO only

def ex0019(): ell(ex0016()._sos[0]).pp() # test that ell does the same thing as previous steps
def ex0019a(): o_svo(ex0016()._sos[0]).pp()
def ex0019b(): o_sov(ex0016()._sos[0]).pp()

## the copy language over {a,b}
gxx = [
    LI(("a",), (("A","Lf"),("C","Lf"))),       # 0
    LI(("b",), (("B","Lf"),("C","Lf"))),       # 1
    LI(("a",), (("C","Rt"),("A","Rt"))),       # 2
    LI(("b",), (("C","Rt"),("B","Rt"))),       # 3
    LI(    (), ((),("C","Rt","Lf"))),          # 4
    LI(    (), (("C","Rt","Lf"),("C",)))       # 5
    ]

def ex01(): ppMg(gxx)

## deriving complete aa from gxx requires 7 merges, with remnant movement
def ex0100(): return d([gxx[2].to_ws(), gxx[4].to_ws()])  # a
def ex0101(): ex0100().pp()
def ex0102(): return d([ex0100(), gxx[4].to_ws()])
def ex0103(): ex0102().pp()
def ex0104(): return d([gxx[0].to_ws(), ex0102()])   # a
def ex0105(): ex0104().pp()
def ex0106(): return d([ex0104(), gxx[4].to_ws()])
def ex0107(): ex0106().pp()
def ex0108(): return d([gxx[5].to_ws(), ex0106()])
def ex0109(): ex0108().pp()
def ex0110(): return d([ex0108(), ex0102()])
def ex0111(): ex0110().pp()
def ex0112(): return d([ex0110(), ex0106()])
def ex0113(): ex0112().pp()
# def ex0113a(): ppSO (o_svo((head.fst) ex0112))

## deriving complete abab, we continue from ex0106
def ex0114(): return d([gxx[3].to_ws(), ex0106()])   # b a a
def ex0115(): ex0114().pp()
def ex0116(): return d([ex0114(),ex0102()])
def ex0117(): ex0116().pp()
def ex0118(): return d([gxx[1].to_ws(), ex0116()])   # b b a a
def ex0119(): ex0118().pp()
def ex0120(): return d([ex0118(),ex0106()])
def ex0121(): ex0120().pp()
def ex0122(): return d([gxx[5].to_ws(), ex0120()])
def ex0123(): ex0122().pp()
def ex0124(): return d([ex0122(),ex0116()])
def ex0125(): ex0124().pp()
def ex0126(): return d([ex0124(),ex0120()])
def ex0127(): ex0126().pp()

## examples from \S1.3.3 of the paper: replicating Stabler (2001: \S2.1)
g133 = [
  LI((), (('T',),('C',))),
  LI((), (('T','Wh'),('C',))),
  LI(('-s',), (('Modal','K'),('T',))),
  LI(('-s',), (('Have','K'),('T',))),
  LI(('-s',), (('Be','K'),('T',))),
  LI(('-s',), (('Vx','K'),('T',))),

  LI(('will',), (('Have',),('Modal',))),
  LI(('will',), (('Be',),('Modal',))),
  LI(('will',), (('Vx',),('Modal',))),

  LI(('have',), (('Been',),('Have',))),
  LI(('have',), (('Ven',),('Have',))),

  LI(('be',), (('Ving',),('Be',))),
  LI(('been',), (('Ving',),('Been',))),

  LI((), (('V','D'),('Vx',))),
  LI(('-en',), (('V','D'),('Ven',))),
  LI(('-ing',), (('V','D'),('Ving',))),

  LI(('eat',), (('D','K'),('V',))),
  LI(('laugh',), ((),('V',))),

  LI(('the',), (('N',),('D','K'))),
  LI(('which',), (('N',),('D','K','Wh'))),

  LI(('king',), ((),('N',))),
  LI(('pie',), ((),('N',)))
  ]

def ex02(): ppMg(g133)

def ex0201():
  return SO([
    SO([
      LI(("which",), (("N",), ("D","K","Wh"))),
      LI(("pie",), ((), ("N",))) ]),
    SO([
      LI(("+",), (("T","Wh"),("C",))),
      SO([
        SO([
          LI(("the",), (("N",), ("D","K"))),
          LI(("king",), ((), ("N",))) ]),
        SO([
          LI(("+s",), (("Have","K"), ("T",))),
          SO([
            LI(("have",), (("Been",), ("Have",))),
            SO([
              LI(("been",), (("Ving",), ("Been",))),
              SO([
                SO([
                  LI(("the",), (("N",), ("D","K"))),
                  LI(("king",), ((), ("N",))) ]),
                SO([
                  LI(("+ing",), (("V","D"), ("Ving",))),
                  SO([
                    SO([
                      LI(("which",), (("N",), ("D","K","Wh"))),
                      LI(("pie",), ((), ("N",))) ]),
                    SO([
                      LI(("eat",), (("D","K"), ("V",))),
                      SO([
                        LI(("which",), (("N",),("D","K","Wh"))),
                        LI(("pie",), ((), ("N",)))]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

def ex0202(): ex0201().pp()
def ex0203(): ell(ex0201()).pp()
def ex0203a(): o_svo(ex0201()).pp() # head movement
def ex0203b(): o_sov(ex0201()).pp() # head movement
#ex0203()
def ex0204(): return h(0,ex0201())[1]
def ex0204a(): ex0204().pp() # head movement
def ex0204b(): o_svo(ex0204()).pp() # head movement
def ex0204c(): o_sov(ex0204()).pp() # head movement
#ex0204b()

## Example of Figure 2, demonstrating multiple occurrences
def ex08(): return SO([
    LI((), (("T",),("C",))),
    SO([
      LI(("the man",), ((),("D", "K", "Scr"))),
      SO([
        LI(("the man",), ((),("D", "K", "Scr"))),
        SO([
          LI((), (("V", "K", "Scr"),("T",))),
          SO([
            LI(("the man",), ((),("D", "K", "Scr"))),
            SO([
              LI(("carefully",), (("V","Scr"), ("V",))),
              SO([
                LI(("the man",), ((),("D", "K", "Scr"))),
                SO([
                  LI(("the man",), ((),("D", "K", "Scr"))),
                  SO([
                    LI((), (("Vx","K","D"), ("V",))),
                    SO([
                      LI(("praises",), (("D",), ("Vx",))),
                      LI(("the man",), ((),("D", "K", "Scr"))) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

# ex08().pp()
def ex08a(): ell(ex08()).pp()
def ex08b(): o_svo(ex08()).pp()
def ex08c(): o_sov(ex08()).pp()

## this example is from Figure 3
def ex09(): return SO([
  LI(("",), (("T",), ("C",))),
  SO([
    LI(("I",), ((),("D", "K"))),
    SO([
      LI(("",), (("V","K"), ("T",))),
      SO([
        LI(("I",), ((),("D", "K"))),
        SO([
          LI(("wonder",), (("C","D"), ("V",))),
          SO([
            SO([
              LI(("",), (("T","Wh"), ("C",))),
              SO([
                SO([
                  LI(("+s",), (("V","K"), ("T",))),
                  SO([
                    LI(("be",), (("A",), ("V",))),
                    SO([
                    LI(("how",), (("A",), ("A","Wh"))),
                      SO([
                      LI(("likely",), (("T",),("A",))),
                        SO([
                          LI(("to",), (("V",), ("T",))),
                          SO([
                            LI(("win",), (("D",), ("V",))),
                            LI(("John",), ((),("D", "K"))) ]) ]) ]) ]) ]) ]),
                LI(("John",), ((),("D", "K"))) ]) ]),
            SO([
              LI(("how",), (("A",), ("A","Wh"))),
              SO([
                LI(("likely",), (("T",),("A",))),
                SO([
                  LI(("to",), (("V",), ("T",))),
                  SO([
                    LI(("win",), (("D",), ("V",))),
                    LI(("John",), ((),("D", "K"))) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

#ex09().pp()
def ex09a(): ell(ex09()).pp()
def ex09b(): o_svo(ex09()).pp()
def ex09c(): o_sov(ex09()).pp()

## this example is from Figure 4
g121 = [
    LI((), (("T",), ("C",))),
    LI((), (("V",), ("T",))),
    LI((), (("Pred", "D"), ("Predx",))),
    LI(("is",), (("A",), ("V",))),
    LI(("cuma",), (("Predx",), ("A",))),
    LI(("e",), ((), ("D",))),
    LI(("na",), (("D",), ("Pred",))),
    LI(("shamhradh",), ((), ("D",))),
    LI(("fhomhar",), ((), ("D",))),
    LI(("gheimhread",), ((), ("D",))),
    LI(("no",), (("Pred", "Pred+"), ("Pred",)))
    ]

def ex20(): return SO([
  LI(("na",), (("D",), ("Pred",))),
  LI(("gheimhread",), ((), ("D",))) ])

def ex21(): return SO([
    LI(("no",), (("Pred", "Pred+"), ("Pred",))),
    ex20() ])

def ex21a(): ell(ex21()).pp()
#ex21a()

def ex22(): return SO([
    SO([
      LI(("na",), (("D",), ("Pred",))),
      LI(("shamhradh",), ((), ("D",))) ]),
    SO([
      LI(("na",), (("D",), ("Pred",))),
      LI(("fhomhar",), ((), ("D",))) ]),
    ex21() ])

#ex22().pp()
def ex22a(): ell(ex22()).pp()
def ex22b(): o_svo(ex22()).pp()
def ex22c(): o_sov(ex22()).pp()

## This is example (10) of Figure 4 but with identical coordinates
def ex23(): return SO([
    SO([
      LI(("na",), (("D",), ("Pred",))),
      LI(("gheimhread",), ((), ("D",))) ]),
    SO([
      LI(("na",), (("D",), ("Pred",))),
      LI(("gheimhread",), ((), ("D",))) ]),
    ex21() ])

#ex23().pp()
def ex23a(): ell(ex23()).pp()
#ex23a()
def ex23b(): o_svo(ex23()).pp()
def ex23c(): o_sov(ex23()).pp()

## this example is from Figure 5
def ex24(): return SO([
    LI((), (("V",),("C",))),
    SO([
      LI(("Jo",), ((),("D",))),
      SO([
        LI(("likes",), (("D", "D"), ("V",))),
        SO([
          LI(("blueberries",), ((), ("D",))),
          LI(("bayberries",), ((), ("D",))),
          LI(("raspberries",), ((), ("D",))),
          LI(("mulberries",), ((), ("D",))),
          SO([
            LI(("and",), (("D","D+"),("D",))),
            LI(("brambleberries",), ((), ("D",))) ]) ]) ]) ]) ])

#ex24().pp()
def ex24a(): ell(ex24()).pp()
#ex24a()
def ex24b(): o_svo(ex24()).pp()
def ex24c(): o_sov(ex24()).pp()
#ex24c()

## atb wh movement -- an English approximation to Figure 6, left
def ex25():
  return SO([
    LI(("who",), ((), ("D","Wh"))),
    SO([
      LI((), (("V","Wh"),("C",))),
      SO([
        SO([
          LI(("Maria",), ((), ("D",))),
          SO([
            LI(("likes",), (("D","D+"),("V",))),
            LI(("who",), ((), ("D","Wh"))) ]) ]),
        SO([
          LI(("and",), (("V","V+"),("V",))),
          SO([
            LI(("Ewa",), ((), ("D",))),
            SO([
              LI(("hates",), (("D","D+"),("V",))),
              LI(("who",), ((), ("D","Wh"))) ]) ]) ]) ]) ]) ])

#ex25().pp()
def ex25a(): ell(ex25()).pp()
#ex25a()
def ex25b(): o_svo(ex25()).pp()
def ex25c(): o_sov(ex25()).pp()

## atb wh movement -- any number of coordinates OK, extending the previous example
def ex26(): return SO([
    LI(("who",), ((), ("D","Wh"))),
    SO([
      LI((), (("V","Wh"),("C",))),
      SO([
        SO([
          LI(("Maria",), ((), ("D",))),
          SO([
            LI(("likes",), (("D","D+"),("V",))),
            LI(("who",), ((), ("D","Wh"))) ]) ]),
        SO([
          LI(("Max",), ((), ("D",))),
          SO([
            LI(("tolerates",), (("D","D+"),("V",))),
            LI(("who",), ((), ("D","Wh"))) ]) ]),
        SO([
          LI(("Zuzanna",), ((), ("D",))),
          SO([
            LI(("pities",), (("D","D+"),("V",))),
            LI(("who",), ((), ("D","Wh"))) ]) ]),
        SO([
          LI(("and",), (("V","V+"),("V",))),
          SO([
            LI(("Ewa",), ((), ("D",))),
            SO([
              LI(("hates",), (("D","D+"),("V",))),
              LI(("who",), ((), ("D","Wh"))) ]) ]) ]) ]) ]) ])

#ex26().pp()
def ex26a(): ell(ex26()).pp()
#ex26a()
def ex26b(): o_svo(ex26()).pp()
def ex26c(): o_sov(ex26()).pp()

# Javanese-like multiple head movement
def ex1201():
  return SO([
    LI(("++",), (("Vgelem",),("C",))),
    SO([
      LI(("Tono",), ((),("D",))),
      SO([
        LI(("want",), (("Visa","D"), ("Vgelem",))),
        SO([
          LI(("can",), (("V",),("Visa",))),
          SO([
            LI(("speak",), (("D",),("V",))),
            LI(("English",), ((),("D",))) ]) ]) ]) ]) ])

def ex1202(): ex1201().pp() # SO
def ex1203(): ell(ex1201()).pp() # WS
def ex1204(): h(0,ex1201())[1].pp() # SO with head movement
def ex1204a(): o_svo(h(0,ex1201())[1]).pp()
def ex1204b(): o_sov(h(0,ex1201())[1]).pp()
#ex1204a()
#ex0204b()
