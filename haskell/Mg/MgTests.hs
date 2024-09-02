module MgTests where           -- Multiset needed. E.g., start ghci with: ghci -package multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List (partition)
import Mg
import MgH
import MgO

-- create lexical workspace
lexWS :: Lex -> WS
lexWS lex = ([L lex], [snd lex])

-- print n spaces
tab 0 = putStr ""
tab n = do { putStr " "; tab (n-1) }

-- join list of strings, separated by sep
joinstr sep = foldr (\x y -> if length y == 0 then x else (x ++ sep ++ y)) ""

-- convert label to pretty string
label2str ([],[]) = "T"
label2str ([],p) = (joinstr "." p)
label2str ( n,p) = (joinstr "." n) ++ " -o " ++ (joinstr "." p)

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

-- pretty print a workspace
ppWS ([],[]) = putStrLn ""
ppWS (so:sos, label:labels) = do { ppSO so ; putStrLn (label2str label); ppWS (sos,labels) }

-- example grammar from section 1.1.2
g112 :: [Lex]
g112 = [
    ([], (["V"],["C"])),
    ([], (["V","Wh"],["C"])),
    (["Jo"], ([],["D"])),
    (["the"], (["N"],["D"])),
    (["which"], (["N"],["D","Wh"])),
    (["who"], ([],["D","Wh"])),
    (["cat"], ([],["N"])),
    (["dog"], ([],["N"])),
    (["food"], ([],["N"])),
    (["likes"],(["D","D"],["V"])),
    (["knows"], (["C","D"],["V"])),
    (["and"], (["D","D+"],["D"])),
    (["or"], (["D","D+"],["D"]))
    ]

ex00 = ppMg g112

ex000 = d [lexWS (g112!!3), lexWS (g112!!6)]  -- the cat
ex001 = ppWS ex000

ex002 = d [lexWS (g112!!4), lexWS (g112!!8)]  -- which food
ex003 = ppWS ex002

ex004 = d [lexWS (g112!!9), ex002] -- likes which food
ex005 = ppWS ex004

ex006 = d [ex004, ex000]  -- the cat likes which food
ex007 = ppWS ex006

ex008 = d [ex006, lexWS (g112!!1)]  -- C[+wh] the cat likes which food
ex009 = ppWS ex008

ex0010 = d [ex008, ex002]  -- which food C[+wh] the cat likes which food
ex0011 = ppWS ex0010

ex0012 = d [lexWS (g112!!10), ex0010]  -- knows which food C[+wh] the cat likes which food
ex0013 = ppWS ex0012

ex0014 = d [ex0012, lexWS (g112!!2)]  -- Jo knows which food C[+wh] the cat likes which food
ex0015 = ppWS ex0014

-- This is the example in Figure 1, with wh movement
ex0016 = d [lexWS (g112!!0), ex0014]  -- C Jo knows which food C[+wh] the cat likes which food
ex0017 = ppWS ex0016
ex0018 = ppSO ((head.fst) ex0016)
ex0019 = ppWS (ell ((head.fst) ex0016))
ex0019a = ppSO (o_svo ((head.fst) ex0016))
ex0019b = ppSO (o_sov ((head.fst) ex0016))

gxx :: [Lex]
gxx = [
    (["a"], (["A","Lf"],["C","Lf"])),       -- 0
    (["b"], (["B","Lf"],["C","Lf"])),       -- 1
    (["a"], (["C","Rt"],["A","Rt"])),       -- 2
    (["b"], (["C","Rt"],["B","Rt"])),       -- 3
    (   [], ([],["C","Rt","Lf"])),            -- 4
    (   [], (["C","Rt","Lf"],["C"]))        -- 5
    ]

ex01 = ppMg gxx

-- deriving complete aa from gxx requires 7 merges, with remnant movement
ex0100 = d [lexWS (gxx!!2), lexWS (gxx!!4)]  -- a right
ex0101 = ppWS ex0100
ex0102 = d [ex0100, lexWS (gxx!!4)] -- MOVE
ex0103 = ppWS ex0102
ex0104 = d [lexWS (gxx!!0), ex0102]   -- a left
ex0105 = ppWS ex0104
ex0106 = d [ex0104, lexWS (gxx!!4)] -- MOVE
ex0107 = ppWS ex0106 -- 
ex0108 = d [lexWS (gxx!!5), ex0106]
ex0109 = ppWS ex0108
ex0110 = d [ex0108, ex0102] -- MOVE
ex0111 = ppWS ex0110
ex0112 = d [ex0110, ex0106] -- MOVE
ex0113 = ppWS ex0112
ex0113a = ppSO (o_svo ((head.fst) ex0112))

-- deriving complete abab, we continue from ex0106
ex0114 = d [lexWS (gxx!!3), ex0106]   -- b a a
ex0115 = ppWS ex0114
ex0116 = d [ex0114, ex0102]  -- MOVE
ex0117 = ppWS ex0116
ex0118 = d [lexWS (gxx!!1), ex0116]   -- b b a a
ex0119 = ppWS ex0118
ex0120 = d [ex0118,ex0106] -- MOVE
ex0121 = ppWS ex0120 -- 
ex0122 = d [lexWS (gxx!!5), ex0120]
ex0123 = ppWS ex0122
ex0124 = d [ex0122,ex0116] -- MOVE
ex0125 = ppWS ex0124
ex0126 = d [ex0124,ex0120] -- MOVE
ex0127 = ppWS ex0126

ex0128 = ppSO ((head.fst) ex0126)
ex0129 = ppWS (ell ((head.fst) ex0126))
ex0129a = ppSO (o_svo ((head.fst) ex0126))

-- examples from \S1.3.3 of the paper: replicating Stabler (2001: \S2.1)
g133 :: [Lex]
g133 = [
    ([], (["T"],["C"])),
    ([], (["T","Wh"],["C"])),

    (["-s"], (["Modal","K"],["T"])),
    (["-s"], (["Have","K"],["T"])),
    (["-s"], (["Be","K"],["T"])),
    (["-s"], (["v","K"],["T"])),

    (["will"], (["Have"],["Modal"])),
    (["will"], (["Be"],["Modal"])),
    (["will"], (["v"],["Modal"])),

    (["have"], (["Been"],["Have"])),
    (["have"], (["Ven"],["Have"])),

    (["be"], (["Ving"],["Be"])),
    (["been"], (["Ving"],["Been"])),

    ([], (["V","D"],["v"])),
    (["-en"], (["V","D"],["Ven"])),
    (["-ing"], (["V","D"],["Ving"])),

    (["eat"], (["D","K"],["V"])),
    (["laugh"], ([],["V"])),

    (["the"], (["N"],["D","K"])),
    (["which"], (["N"],["D","K","Wh"])),

    (["king"], ([],["N"])),
    (["pie"], ([],["N"]))
    ]

ex02 = ppMg g133

ex0201 :: SO
ex0201 = S (MultiSet.fromList [
          S (MultiSet.fromList [
            L (["which"], (["N"],["D","K","Wh"])),
            L (["pie"], ([], ["N"])) ]),
          S (MultiSet.fromList [
            L (["+"], (["T","Wh"],["C"])),
            S (MultiSet.fromList [
              S (MultiSet.fromList [
                L (["the"], (["N"],["D","K"])),
                L (["king"], ([], ["N"])) ]),
              S (MultiSet.fromList [
                L (["+s"], (["Have","K"],["T"])),
                S (MultiSet.fromList [
                  L (["have"], (["Been"],["Have"])),
                  S (MultiSet.fromList [
                    L (["been"], (["Ving"],["Been"])),
                    S (MultiSet.fromList [
                      S (MultiSet.fromList [
                        L (["the"], (["N"],["D","K"])),
                        L (["king"], ([], ["N"])) ]),
                      S (MultiSet.fromList [
                        L (["+ing"], (["V","D"],["Ving"])),
                        S (MultiSet.fromList [
                          S (MultiSet.fromList [
                             L (["which"], (["N"],["D","K","Wh"])),
                             L (["pie"], ([], ["N"])) ]),
                          S (MultiSet.fromList [
                            L (["eat"], (["D","K"],["V"])),
                            S (MultiSet.fromList [
                               L (["which"], (["N"],["D","K","Wh"])),
                               L (["pie"], ([], ["N"])) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

ex0202 = ppSO ex0201
ex0203 = ppWS (ell ex0201)
ex0204 = ppSO (snd (h 0 ex0201)) -- head movement
ex0204a = ppSO (o_svo (snd (h 0 ex0201))) -- head movement
ex0204b = ppSO (o_sov (snd (h 0 ex0201))) -- head movement

-- Example of Figure 2, demonstrating multiple occurrences
ex08 :: SO
ex08 = S (MultiSet.fromList [
        L ([], (["T"],["C"])),
        S (MultiSet.fromList
         [ L (["the man"], ([],["D", "K", "Scr"])),
           S (MultiSet.fromList
             [ L (["the man"], ([],["D", "K", "Scr"])),
             S (MultiSet.fromList
               [ L ([], (["V", "K", "Scr"],["T"])),
               S (MultiSet.fromList
                 [ L (["the man"], ([],["D", "K", "Scr"])),
                 S (MultiSet.fromList [
                   L (["carefully"], (["V","Scr"], ["V"])),
                   S (MultiSet.fromList
                     [ L (["the man"], ([],["D", "K", "Scr"])),
                     S (MultiSet.fromList [
                       L (["the man"], ([],["D", "K", "Scr"])),
                       S (MultiSet.fromList [
                         L ([], (["v","K","D"], ["V"])),
                         S (MultiSet.fromList [
                           L (["praises"], (["D"], ["v"])),
                           L (["the man"], ([],["D", "K", "Scr"])) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

ex08a = ppWS (ell ex08)
ex08b = ppSO (o_svo ex08)
ex08c = ppSO (o_sov ex08)

-- this example is from Figure 3
ex09 :: SO
ex09 =
 S (MultiSet.fromList [
   L ([""], (["T"], ["C"])),
   S (MultiSet.fromList [
     L (["I"], ([],["D", "K"])),
     S (MultiSet.fromList [
       L ([""], (["V","K"], ["T"])),
       S (MultiSet.fromList [
         L (["I"], ([],["D", "K"])),
         S (MultiSet.fromList [
           L (["wonder"], (["C","D"], ["V"])),
           S (MultiSet.fromList [
             S (MultiSet.fromList [
               L ([""], (["T","Wh"], ["C"])),
               S (MultiSet.fromList [
                 S (MultiSet.fromList [
                   L (["+s"], (["V","K"], ["T"])),
                   S (MultiSet.fromList [
                     L (["be"], (["A"], ["V"])),
                     S (MultiSet.fromList [
                       L (["how"], (["A"], ["A","Wh"])),
                       S (MultiSet.fromList [
                         L (["likely"], (["T"],["A"])),
                          S (MultiSet.fromList [
                            L (["to"], (["V"], ["T"])),
                            S (MultiSet.fromList [
                              L (["win"], (["D"], ["V"])),
                              L (["John"], ([],["D", "K"])) ]) ]) ]) ]) ]) ]),
               L (["John"], ([],["D", "K"])) ]) ]),
           S (MultiSet.fromList [
             L (["how"], (["A"], ["A","Wh"])),
             S (MultiSet.fromList [
               L (["likely"], (["T"],["A"])),
               S (MultiSet.fromList [
                 L (["to"], (["V"], ["T"])),
                 S (MultiSet.fromList [
                   L (["win"], (["D"], ["V"])),
                   L (["John"], ([],["D", "K"])) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

ex09a = ppWS (ell ex09)
ex09b = ppSO (o_svo ex09)
ex09c = ppSO (o_sov ex09)

-- this example is from Figure 4
g121 :: [Lex]
g121 = [
    ([], (["T"], ["C"])),
    ([], (["V"], ["T"])),
    ([], (["Pred", "D"], ["pred"])),
    (["is"], (["A"], ["V"])),
    (["cuma"], (["pred"], ["A"])),
    (["e"], ([], ["D"])),
    (["na"], (["D"], ["Pred"])),
    (["shamhradh"], ([], ["D"])),
    (["fhomhar"], ([], ["D"])),
    (["gheimhread"], ([], ["D"])),
    (["no"], (["Pred", "Pred+"], ["Pred"]))
    ]

ex20 :: SO
ex20 = S (MultiSet.fromList [
            L (["na"], (["D"], ["Pred"])),
            L (["gheimhread"], ([], ["D"])) ])

ex21 :: SO
ex21 = S (MultiSet.fromList [
            L (["no"], (["Pred", "Pred+"], ["Pred"])),
            ex20 ])

ex22 :: SO
ex22 = S (MultiSet.fromList [
             S (MultiSet.fromList [
                   L (["na"], (["D"], ["Pred"])),
                   L (["shamhradh"], ([], ["D"])) ]),
             S (MultiSet.fromList [
                   L (["na"], (["D"], ["Pred"])),
                   L (["fhomhar"], ([], ["D"])) ]),
             ex21 ])

ex22a = ppWS (ell ex22)
ex22b = ppSO (o_svo ex22)
ex22c = ppSO (o_sov ex22)

-- This is example (10) of Figure 4 but with identical coordinates
ex23 :: SO
ex23 = S (MultiSet.fromList [
             S (MultiSet.fromList [
                   L (["na"], (["D"], ["Pred"])),
                   L (["gheimhread"], ([], ["D"])) ]),
             S (MultiSet.fromList [
                   L (["na"], (["D"], ["Pred"])),
                   L (["gheimhread"], ([], ["D"])) ]),
             ex21 ])

ex23a = ppWS (ell ex23)
ex23b = ppSO (o_svo ex23)
ex23c = ppSO (o_sov ex23)

-- this example is from Figure 5
ex24 :: SO
ex24 = S (MultiSet.fromList
        [ L ([], (["V"],["C"])),
          S (MultiSet.fromList
           [ L (["Jo"], ([],["D"])),
             S (MultiSet.fromList [
               L (["likes"], (["D", "D"], ["V"])),
               S (MultiSet.fromList [
                L (["blueberries"], ([], ["D"])),
                L (["bayberries"], ([], ["D"])),
                L (["raspberries"], ([], ["D"])),
                L (["mulberries"], ([], ["D"])),
                S (MultiSet.fromList [
                  L (["and"], (["D","D+"],["D"])),
                  L (["brambleberries"], ([], ["D"])) ]) ]) ]) ]) ])

ex24a = ppWS (ell ex24)
ex24b = ppSO (o_svo ex24)
ex24c = ppSO (o_sov ex24)

-- this example is (an English approximation to) Figure 6, left -- atb wh movement
ex25 :: SO
ex25 = S (MultiSet.fromList [
         L (["who"], ([], ["D","Wh"])),
         S (MultiSet.fromList [
           L ([], (["V","Wh"],["C"])),
           S (MultiSet.fromList [
             S (MultiSet.fromList [
               L (["Maria"], ([], ["D"])),
               S (MultiSet.fromList [
                 L (["likes"], (["D","D+"],["V"])),
                 L (["who"], ([], ["D","Wh"])) ]) ]),
             S (MultiSet.fromList [
               L (["and"], (["V","V+"],["V"])),
               S (MultiSet.fromList [
                 L (["Ewa"], ([], ["D"])),
                 S (MultiSet.fromList [
                   L (["hates"], (["D","D+"],["V"])),
                   L (["who"], ([], ["D","Wh"])) ]) ]) ]) ]) ]) ])

ex25a = ppWS (ell ex25)
ex25b = ppSO (o_svo ex25)
ex25c = ppSO (o_sov ex25)

-- we can have ATB with any number of coordinates, extending the previous example
ex26 :: SO
ex26 = S (MultiSet.fromList [
         L (["who"], ([], ["D","Wh"])),
         S (MultiSet.fromList [
           L ([], (["V","Wh"],["C"])),
           S (MultiSet.fromList [
             S (MultiSet.fromList [
               L (["Maria"], ([], ["D"])),
               S (MultiSet.fromList [
                 L (["likes"], (["D","D+"],["V"])),
                 L (["who"], ([], ["D","Wh"])) ]) ]),
             S (MultiSet.fromList [
               L (["Max"], ([], ["D"])),
               S (MultiSet.fromList [
                 L (["tolerates"], (["D","D+"],["V"])),
                 L (["who"], ([], ["D","Wh"])) ]) ]),
             S (MultiSet.fromList [
               L (["Zuzanna"], ([], ["D"])),
               S (MultiSet.fromList [
                 L (["pities"], (["D","D+"],["V"])),
                 L (["who"], ([], ["D","Wh"])) ]) ]),
             S (MultiSet.fromList [
               L (["and"], (["V","V+"],["V"])),
               S (MultiSet.fromList [
                 L (["Ewa"], ([], ["D"])),
                 S (MultiSet.fromList [
                   L (["hates"], (["D","D+"],["V"])),
                   L (["who"], ([], ["D","Wh"])) ]) ]) ]) ]) ]) ])

ex26a = ppWS (ell ex26)
ex26b = ppSO (o_svo ex26)
ex26c = ppSO (o_sov ex26)

-- Javanese-like multiple head movement
ex1201 :: SO
ex1201 = S (MultiSet.fromList [
           L (["++"], (["Vgelem"],["C"])),
           S (MultiSet.fromList [
             L (["Tono"], ([],["D"])),
             S (MultiSet.fromList [
               L (["want"], (["Visa","D"], ["Vgelem"])),
               S (MultiSet.fromList [
                 L (["can"], (["V"],["Visa"])),
                 S (MultiSet.fromList [
                   L (["speak"], (["D"],["V"])),
                   L (["English"], ([],["D"])) ]) ]) ]) ]) ])

ex1202 = ppSO ex1201
ex1203 = ppWS (ell ex1201)
ex1204 = ppSO (snd (h 0 ex1201)) -- head movement
ex1204a = ppSO (o_svo (snd (h 0 ex1201))) -- head movement
ex1204b = ppSO (o_sov (snd (h 0 ex1201))) -- head movement

-- head movement of do
ex27 :: SO
ex27 = S (MultiSet.fromList [
         L (["who"], ([],["D","K","Wh"])),
         S (MultiSet.fromList [
           L (["+"], (["T","Wh"],["C"])),
           S (MultiSet.fromList [
             L (["Maria"], ([],["D","K"])),
             S (MultiSet.fromList [
               L (["+s"], (["Do","K"],["T"])),
                 S (MultiSet.fromList [
                   L (["Maria"], ([],["D","K"])),
                   S (MultiSet.fromList [
                     L (["do"], (["V","D"],["Do"])),
                     S (MultiSet.fromList [
                       L (["who"], ([],["D","K","Wh"])),
                       S (MultiSet.fromList [
                         L (["like"], (["D","K"],["V"])),
                         L (["who"], ([],["D","K","Wh"])) ]) ]) ]) ]) ]) ]) ]) ])


ex27a = ppWS (ell ex27)
ex27b = ppSO (o_svo ex27)
ex27c = ppSO (o_sov ex27)

-- ATB head movement of do+s
ex28 :: SO
ex28 = S (MultiSet.fromList [
         L (["who"], ([],["D","K","Wh"])),
         S (MultiSet.fromList [
           L (["+"], (["T","Wh"],["C"])),
           S (MultiSet.fromList [

             S (MultiSet.fromList [
               L (["Jack"], ([],["D","K"])),
               S (MultiSet.fromList [
                 L (["+s"], (["Do","K"],["T"])),
                   S (MultiSet.fromList [
                     L (["Jack"], ([],["D","K"])),
                     S (MultiSet.fromList [
                       L (["do"], (["V","D"],["Do"])),
                       S (MultiSet.fromList [
                         L (["who"], ([],["D","K","Wh"])),
                         S (MultiSet.fromList [
                           L (["praise"], (["D","K"],["V"])),
                           L (["who"], ([],["D","K","Wh"])) ]) ]) ]) ]) ]) ]),

             S (MultiSet.fromList [
               L (["Sue"], ([],["D","K"])),
               S (MultiSet.fromList [
                 L (["+s"], (["Do","K"],["T"])),
                   S (MultiSet.fromList [
                     L (["Sue"], ([],["D","K"])),
                     S (MultiSet.fromList [
                       L (["do"], (["V","D"],["Do"])),
                       S (MultiSet.fromList [
                         L (["who"], ([],["D","K","Wh"])),
                         S (MultiSet.fromList [
                           L (["criticize"], (["D","K"],["V"])),
                           L (["who"], ([],["D","K","Wh"])) ]) ]) ]) ]) ]) ]),

             S (MultiSet.fromList [
                L (["and"], (["T","T+"],["T"])),
                S (MultiSet.fromList [
                  L (["Mary"], ([],["D","K"])),
                  S (MultiSet.fromList [
                    L (["+s"], (["Do","K"],["T"])),
                      S (MultiSet.fromList [
                        L (["Mary"], ([],["D","K"])),
                        S (MultiSet.fromList [
                          L (["do"], (["V","D"],["Do"])),
                          S (MultiSet.fromList [
                            L (["who"], ([],["D","K","Wh"])),
                            S (MultiSet.fromList [
                              L (["ignore"], (["D","K"],["V"])),
                              L (["who"], ([],["D","K","Wh"])) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

ex28a = ppWS (ell ex28)
ex28b = ppSO (snd (h 0 ex28))
ex28c = ppSO (o_svo (snd (h 0 ex28)))
ex28d = ppSO (o_sov (snd (h 0 ex28)))

ex29 =
   S (MultiSet.fromList [
     L ([""], (["T"],["C"])),
     S (MultiSet.fromList [
       L (["he"], ([],["D","K"])),
       S (MultiSet.fromList [
         L (["~s"], (["v","K"],["T"])),
         S (MultiSet.fromList [
           L (["he"], ([],["D","K"])),
           S (MultiSet.fromList [
             L (["+"], (["V","D"],["v"])),
             S (MultiSet.fromList [
               L (["know"], (["C"],["V"])),
               S (MultiSet.fromList [
                 S (MultiSet.fromList [
                   L (["which"], (["N"],["D","K","Wh"])),
                   L (["wine"], ([],["N"])) ]),
                 S (MultiSet.fromList [
                   L (["+"], (["T","Wh"],["C"])),
                   S (MultiSet.fromList [
                     S (MultiSet.fromList [
                       L (["the"], (["N"],["D","K"])),
                       L (["king"], ([],["N"])) ]),
                     S (MultiSet.fromList [
                       L (["~s"], (["v","K"],["T"])),
                       S (MultiSet.fromList [
                         S (MultiSet.fromList [
                           L (["the"], (["N"],["D","K"])),
                           L (["king"], ([],["N"])) ]),
                         S (MultiSet.fromList [
                           L (["+"], (["V","D"],["v"])),
                           S (MultiSet.fromList [
                             S (MultiSet.fromList [
                               L (["which"], (["N"],["D","K","Wh"])),
                               L (["wine"], ([],["N"])) ]),
                             S (MultiSet.fromList [
                               L (["like"], (["D","K"],["V"])),
                               S (MultiSet.fromList [
                                 L (["which"], (["N"],["D","K","Wh"])),
                                 L (["wine"], ([],["N"])) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])
ex29a = ppSO ex29
ex29b = ppWS (ell ex29)
ex29c = ppSO (o_svo (snd (h 0 ex29)))
ex29d = ppSO (o_sov (snd (h 0 ex29)))
