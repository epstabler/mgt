module MgTests where           -- Multiset needed. E.g., start ghci with: stack ghci --package multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List
import qualified Data.List as List

import Mg
import MgTransduction
import MgLinearization

{--- IO: prettyPrint various structures ---}
tab 0 = putStr ""
tab n = do { putStr " "; tab (n-1) }

-- join list of strings, separated by sep
joinstr sep = foldr (\x y -> if length y == 0 then x else (x ++ sep ++ y)) ""

-- convert feature to pretty string
ft2str (One f) = show f
ft2str (Plus f) = (show f) ++ "+"

-- convert label to pretty string
label2str ([],[]) = "T"
label2str ([],p) = (joinstr "." (map ft2str p))
label2str ( n,p) = (joinstr "." (map ft2str n)) ++ " -o " ++ (joinstr "." (map ft2str p))

-- convert lexical item to pretty string
lex2str ([],f) = "([], " ++ (label2str f) ++ ")"
lex2str (s,f) = "(" ++ joinstr " " s ++ ", " ++ (label2str f) ++ ")"

-- pretty print grammar
ppMg = mapM_ (putStrLn.lex2str)

-- convert SO to pretty string
so2str (S so) = "{" ++ (joinstr ", " (map so2str (MultiSet.elems so))) ++ "}"
so2str (L (w,label)) = "(" ++ (joinstr " " w) ++ ", " ++ (label2str label) ++ ")"

-- pretty print SO
ppSO so = do { ppSO' 0 so ; putStrLn "" }

-- pretty print SO with indent i
ppSO' i (L w) = do { putStr (so2str (L w)) }
ppSO' i (S so) = do { putStr "{ " ; ppSOs (i+2) (MultiSet.elems so) ; putStr " }" }
ppSO' i (O t) = do { putStr "[ " ; ppPh' (i+2) t ; putStr " ]" }

-- pretty print PhTree
ppPh t = do { ppPh' 0 t ; putStrLn "" }

-- pretty print PhTree with indent i
ppPh' i (Pl w) = do { putStr (so2str (L w)) }
ppPh' i (Ps t) = do { putStr "[ " ; ppPhs (i+2) t ; putStr " ]" }

-- pretty print a list of PhTrees with indent i
ppPhs i [] = putStr ""
ppPhs i (x:[]) = do { ppPh' i x }
ppPhs i (x:xs) = do { ppPh' i x ; putStrLn "," ; tab i ; ppPhs i xs }

-- pretty print a list of SOs with indent i
ppSOs i [] = putStr ""
ppSOs i (x:[]) = do { ppSO' i x }
ppSOs i (x:xs) = do { ppSO' i x ; putStrLn "," ; tab i ; ppSOs i xs }

-- pretty print LSO
ppLSO lso = do { ppLSO' 0 lso ; putStrLn "" }

-- pretty print LSO with indent i
ppLSO' i ((L w), label) = do { putStr (so2str (L w)); putStr ", "; putStr (label2str label) }
ppLSO' i ((S so),label) = do { putStr "{ " ; ppSOs (i+2) (MultiSet.elems so) ; putStr " },"; putStr (label2str label) }
ppLSO' i ((O t),label) = do { ppPh t; putStr (label2str label) }

-- pretty print a list of SOs with indent i
ppLSOs i [] = putStr ""
ppLSOs i (x:[]) = do { ppLSO' i x }
ppLSOs i (x:xs) = do { ppLSO' i x ; putStrLn "," ; tab i ; ppLSOs i xs }

-- pretty print a workspace with indent i
ppWS ws = do {ppLSOs 0 (MultiSet.toList ws); putStrLn "" }

-- create lexical LSO from SO
lexLSO :: SO -> LSO
lexLSO (L x) = (L x, snd x)

-- create lexical WS from SO
lexWS :: SO -> WS
lexWS x = MultiSet.singleton (lexLSO x)

-- example grammar from section 1.1.2
g112 :: [Lex]
g112 = [
    ([], ([One V],[One C])),
    ([], ([One V,One Wh],[One C])),
    (["Jo"], ([],[One D])),
    (["the"], ([One N],[One D])),
    (["which"], ([One N],[One D,One Wh])),
    (["who"], ([],[One D,One Wh])),
    (["cat"], ([],[One N])),
    (["dog"], ([],[One N])),
    (["food"], ([],[One N])),
    (["likes"],([One D,One D],[One V])),
    (["knows"], ([One C,One D],[One V])),
    (["and"], ([One D,Plus D],[One D])),
    (["or"], ([One D,Plus D],[One D]))
    ]

ex00 = ppMg g112

exA = S (MultiSet.fromList [
            L (["likes"],([One D,One D],[One V])),
            L (["who"], ([],[One D,One Wh])) ])

exB = S (MultiSet.fromList [
            L (["Jo"], ([],[One D])),
            exA ])

exC = S (MultiSet.fromList [
            L ([], ([One V,One Wh],[One C])),
            exB ])

ex01 :: SO
ex01 = S (MultiSet.fromList
         [ L ([], ([One V,One Wh],[One C])),
           S (MultiSet.fromList
              [ L (["Jo"], ([],[One D])),
                S (MultiSet.fromList [
                      L (["likes"],([One D,One D],[One V])),
                      L (["who"], ([],[One D,One Wh])) ])
              ])
         ])
ex01a = ppWS (ell ex01)

ex02 :: SO
ex02 = S (MultiSet.fromList [
         L (["which"], ([One N],[One D,One Wh])),
         L (["food"], ([],[One N])) ])

ex03 :: SO
ex03 = S (MultiSet.fromList [
         L ([], ([One V,One Wh],[One C])),
         S (MultiSet.fromList [
           S (MultiSet.fromList [
             L (["the"], ([One N],[One D])),
             L (["cat"], ([],[One N])) ]),
           S (MultiSet.fromList [
             L (["likes"],([One D,One D],[One V])),
             ex02 ]) ]) ])

ex04 :: SO
ex04 = S (MultiSet.fromList [
             ex02,
             ex03 ])

ex05 :: SO
ex05 = S (MultiSet.fromList [
             L (["knows"], ([One C,One D],[One V])),
             ex04 ])

ex06 :: SO
ex06 = S (MultiSet.fromList [
             L (["Jo"], ([],[One D])),
             ex05 ])

-- This is the example in Figure 1
ex07 ::SO
ex07 = S (MultiSet.fromList [
             L ([], ([One V],[One C])),
             ex06 ])

ex07a = ppSO ex07
ex07b = ppWS (ell ex07)
ex07c = ppSO (ord_svo ex07)
ex07d = ppSO (ord_sov ex07)

-- Example of Figure 2, demonstrating multiple occurrences
ex08 = S (MultiSet.fromList [
        L ([], ([One T],[One C])),
        S (MultiSet.fromList
         [ L (["the man"], ([],[One D, One K, One Scr])),
           S (MultiSet.fromList
             [ L (["the man"], ([],[One D, One K, One Scr])),
             S (MultiSet.fromList
               [ L ([], ([One V, One K, One Scr],[One T])),
               S (MultiSet.fromList
                 [ L (["the man"], ([],[One D, One K, One Scr])),
                 S (MultiSet.fromList [
                   L (["carefully"], ([One V,One Scr], [One V])),
                   S (MultiSet.fromList
                     [ L (["the man"], ([],[One D, One K, One Scr])),
                     S (MultiSet.fromList [
                       L (["the man"], ([],[One D, One K, One Scr])),
                       S (MultiSet.fromList [
                         L ([], ([One Vx,One K,One D], [One V])),
                         S (MultiSet.fromList [
                           L (["praises"], ([One D], [One Vx])),
                           L (["the man"], ([],[One D, One K, One Scr])) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

ex08a = ppWS (ell ex08)
ex08b = ppSO (ord_svo ex08)
ex08c = ppSO (ord_sov ex08)

-- this example is from Figure 3
g121 :: [Lex]
g121 = [
    ([], ([One T], [One C])),
    ([], ([One V], [One T])),
    ([], ([One Pred, One D], [One Predx])),
    (["is"], ([One A], [One V])),
    (["cuma"], ([One Predx], [One A])),
    (["e"], ([], [One D])),
    (["na"], ([One D], [One Pred])),
    (["shamhradh"], ([], [One D])),
    (["fhomhar"], ([], [One D])),
    (["gheimhread"], ([], [One D])),
    (["no"], ([One Pred, Plus Pred], [One Pred]))
    ]

ex20 :: SO
ex20 = S (MultiSet.fromList [
            L (["na"], ([One D], [One Pred])),
            L (["gheimhread"], ([], [One D])) ])

ex21 :: SO
ex21 = S (MultiSet.fromList [
            L (["no"], ([One Pred, Plus Pred], [One Pred])),
            ex20 ])

ex22 :: SO
ex22 = S (MultiSet.fromList [
             S (MultiSet.fromList [
                   L (["na"], ([One D], [One Pred])),
                   L (["shamhradh"], ([], [One D])) ]),
             S (MultiSet.fromList [
                   L (["na"], ([One D], [One Pred])),
                   L (["fhomhar"], ([], [One D])) ]),
             ex21 ])

ex22a = ppWS (ell ex22)
ex22b = ppSO (ord_svo ex22)
ex22c = ppSO (ord_sov ex22)

-- This is example (10) of Figure 3 but with identical coordinates
ex23 :: SO
ex23 = S (MultiSet.fromList [
             S (MultiSet.fromList [
                   L (["na"], ([One D], [One Pred])),
                   L (["gheimhread"], ([], [One D])) ]),
             S (MultiSet.fromList [
                   L (["na"], ([One D], [One Pred])),
                   L (["gheimhread"], ([], [One D])) ]),
             ex21 ])

ex23a = ppWS (ell ex23)
ex23b = ppSO (ord_svo ex23)
ex23c = ppSO (ord_sov ex23)

-- this example is from Figure 4
ex24 :: SO
ex24 = S (MultiSet.fromList
        [ L ([], ([One V],[One C])),
          S (MultiSet.fromList
           [ L (["Jo"], ([],[One D])),
             S (MultiSet.fromList [
               L (["likes"], ([One D, One D], [One V])),
               S (MultiSet.fromList [
                L (["blueberries"], ([], [One D])),
                L (["bayberries"], ([], [One D])),
                L (["raspberries"], ([], [One D])),
                L (["mulberries"], ([], [One D])),
                S (MultiSet.fromList [
                  L (["and"], ([One D,Plus D],[One D])),
                  L (["brambleberries"], ([], [One D])) ]) ]) ]) ]) ])

ex24a = ppWS (ell ex24)
ex24b = ppSO (ord_svo ex24)
ex24c = ppSO (ord_sov ex24)

-- this example is (an English approximation to) Figure 5, left
ex25 :: SO
ex25 = S (MultiSet.fromList [
         L (["who"], ([], [One D,One Wh])),
         S (MultiSet.fromList [
           L ([], ([One V,One Wh],[One C])),
           S (MultiSet.fromList [
             S (MultiSet.fromList [
               L (["Maria"], ([], [One D])),
               S (MultiSet.fromList [
                 L (["likes"], ([One D,Plus D],[One V])),
                 L (["who"], ([], [One D,One Wh])) ]) ]),
             S (MultiSet.fromList [
               L (["and"], ([One V,Plus V],[One V])),
               S (MultiSet.fromList [
                 L (["Ewa"], ([], [One D])),
                 S (MultiSet.fromList [
                   L (["hates"], ([One D,Plus D],[One V])),
                   L (["who"], ([], [One D,One Wh])) ]) ]) ]) ]) ]) ])

ex25a = ppWS (ell ex25)
ex25b = ppSO (ord_svo ex25)
ex25c = ppSO (ord_sov ex25)

-- we can have ATB with any number of coordinates, extending the previous example
ex26 :: SO
ex26 = S (MultiSet.fromList [
         L (["who"], ([], [One D,One Wh])),
         S (MultiSet.fromList [
           L ([], ([One V,One Wh],[One C])),
           S (MultiSet.fromList [
             S (MultiSet.fromList [
               L (["Maria"], ([], [One D])),
               S (MultiSet.fromList [
                 L (["likes"], ([One D,Plus D],[One V])),
                 L (["who"], ([], [One D,One Wh])) ]) ]),
             S (MultiSet.fromList [
               L (["Max"], ([], [One D])),
               S (MultiSet.fromList [
                 L (["tolerates"], ([One D,Plus D],[One V])),
                 L (["who"], ([], [One D,One Wh])) ]) ]),
             S (MultiSet.fromList [
               L (["Zuzanna"], ([], [One D])),
               S (MultiSet.fromList [
                 L (["pities"], ([One D,Plus D],[One V])),
                 L (["who"], ([], [One D,One Wh])) ]) ]),
             S (MultiSet.fromList [
               L (["and"], ([One V,Plus V],[One V])),
               S (MultiSet.fromList [
                 L (["Ewa"], ([], [One D])),
                 S (MultiSet.fromList [
                   L (["hates"], ([One D,Plus D],[One V])),
                   L (["who"], ([], [One D,One Wh])) ]) ]) ]) ]) ]) ])

ex26a = ppWS (ell ex26)
ex26b = ppSO (ord_svo ex26)
ex26c = ppSO (ord_sov ex26)

-- example from \S1.3.3 of the paper: replicating Stabler (2001: \S2.1)
--   but without representing the strings of expressions as triples,
--   and with a deterministic free-affix transduction instead of many new derivational rules
g133 :: [Lex]
g133 = [
    ([], ([One T],[One C])),
    ([], ([One T,One Wh],[One C])),

    (["-s"], ([One Modal,One K],[One T])),
    (["-s"], ([One Have,One K],[One T])),
    (["-s"], ([One Be,One K],[One T])),
    (["-s"], ([One Vx,One K],[One T])),

    (["will"], ([One Have],[One Modal])),
    (["will"], ([One Be],[One Modal])),
    (["will"], ([One Vx],[One Modal])),

    (["have"], ([One Been],[One Have])),
    (["have"], ([One Ven],[One Have])),

    (["be"], ([One Ving],[One Be])),
    (["been"], ([One Ving],[One Been])),

    ([], ([One V,One D],[One Vx])),
    (["-en"], ([One V,One D],[One Ven])),
    (["-ing"], ([One V,One D],[One Ving])),

    (["eat"], ([One D,One K],[One V])),
    (["laugh"], ([],[One V])),

    (["the"], ([One N],[One D,One K])),
    (["which"], ([One N],[One D,One K,One Wh])),

    (["king"], ([],[One N])),
    (["pie"], ([],[One N]))
    ]

ex27a = ppMg g133

ex27f = S (MultiSet.fromList [
          S (MultiSet.fromList [
            L (["which"], ([One N],[One D,One K,One Wh])),
            L (["pie"], ([], [One N])) ]),
          S (MultiSet.fromList [
            L ([], ([One T,One Wh],[One C])),
            S (MultiSet.fromList [
              S (MultiSet.fromList [
                L (["the"], ([One N],[One D,One K])),
                L (["king"], ([], [One N])) ]),
              S (MultiSet.fromList [
                L (["-s"], ([One Have,One K],[One T])),
                S (MultiSet.fromList [
                  L (["have"], ([One Been],[One Have])),
                  S (MultiSet.fromList [
                    L (["been"], ([One Ving],[One Been])),
                    S (MultiSet.fromList [
                      S (MultiSet.fromList [
                        L (["the"], ([One N],[One D,One K])),
                        L (["king"], ([], [One N])) ]),
                      S (MultiSet.fromList [
                        L (["-ing"], ([One V,One D],[One Ving])),
                        S (MultiSet.fromList [
                          S (MultiSet.fromList [
                             L (["which"], ([One N],[One D,One K,One Wh])),
                             L (["pie"], ([], [One N])) ]),
                          S (MultiSet.fromList [
                            L (["eat"], ([One D,One K],[One V])),
                            S (MultiSet.fromList [
                               L (["which"], ([One N],[One D,One K,One Wh])),
                               L (["pie"], ([], [One N])) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

ex27g =  ppWS(ell(ex27f))

ex27h = ppSO(ord_svo(ex27f))

-- ex27hm = ppSO(fa(ord_svo(ex27f)))

ex27i = ppSO(ord_sov(ex27f))

-- ex27im = ppSO(fa(ord_sov(ex27f)))
