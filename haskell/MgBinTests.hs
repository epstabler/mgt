
module MgBinTests where
import Data.MultiSet (MultiSet, fromList, toList, elems) -- Multiset needed. E.g. use: ghci -package multiset
import MgBin
import MgBinH
import MgBinO
import MgBinM

-- create lexical workspace
lexWS :: Lex -> WS
lexWS lex = ([L lex], [(fst.snd) lex])

-- print n spaces
tab 0 = putStr ""
tab n = do { putStr " "; tab (n-1) }

-- join list of strings, separated by sep
joinstr sep = foldr (\x y -> if length y == 0 then x else (x ++ sep ++ y)) ""

-- convert label to pretty string
label2str ([],[]) = "[]"
label2str ([],p) = (joinstr "." p)
label2str ( n,p) = (joinstr "." n) ++ " -o " ++ (joinstr "." p)

labelAgr2str (label,("","")) = label2str label
labelAgr2str (label,(phi,"")) = label2str label ++ " -- " ++ phi
labelAgr2str (label,(phi,k)) = label2str label ++ " -- " ++ phi ++ "-" ++ k

-- convert lexical item to pretty string
lex2str ([],f) = "([], " ++ (labelAgr2str f) ++ ")"
lex2str (s,f) = "(" ++ joinstr " " s ++ ", " ++ (labelAgr2str f) ++ ")"

-- pretty print grammar
ppMg = mapM_ (putStrLn.lex2str)

-- example grammar (1) from section 1.1.2
g1 :: [Lex]
g1 = [
    ([], ((["V"],["C"]), ("",""))),                -- 0
    ([], ((["V","Wh"],["C"]), ("",""))),           -- 1
    (["Jo"], (([],["D"]), ("3sg",""))),            -- 2
    (["the"], ((["N"],["D"]), ("3sg",""))),        -- 3
    (["which"], ((["N"],["D","Wh"]), ("3",""))),   -- 4
    (["who"], (([],["D","Wh"]), ("3sg",""))),      -- 5
    (["cat"], (([],["N"]), ("sg",""))),            -- 6
    (["dog"], (([],["N"]), ("sg",""))),            -- 7
    (["food"], (([],["N"]), ("sg",""))),           -- 8
    (["likes"], ((["D","D"],["V"]), ("3sg",""))),  -- 9
    (["knows"], ((["C","D"],["V"]), ("3sg",""))),  -- 10
    (["she"], (([],["D"]), ("3sg","nom"))),        -- 11
    (["he"], (([],["D"]), ("3sg","nom")))          -- 12
    ]

ex00 = ppMg g1

-- convert SO to pretty string
so2str (S so) = "{" ++ (joinstr ", " (map so2str (elems so))) ++ "}"
so2str (L (w,labelAgr)) = "(" ++ (joinstr " " w) ++ ", " ++ (labelAgr2str labelAgr) ++ ")"

-- pretty print SO
ppSO so = do { ppSO' 0 so ; putStrLn "" }

-- pretty print SO with indent i
ppSO' i (L w) = do { putStr (so2str (L w)) }
ppSO' i (S so) = do { putStr "{ " ; ppSOs (i+2) (elems so) ; putStr " }" }
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
ppWS :: WS -> IO ()
ppWS ([],[]) = putStrLn ""
ppWS (so:sos, label:labels) = do { ppSO so ; putStrLn (label2str label); ppWS (sos,labels) }


fig1a00 = d [lexWS (g1!!3), lexWS (g1!!6)]  -- the cat
fig1a00ws = ppWS fig1a00

fig1a01 = d [lexWS (g1!!4), lexWS (g1!!8)]  -- which food
fig1a01ws = ppWS fig1a01

fig1a02 = d [lexWS (g1!!9), fig1a01] -- likes which food
fig1a02ws = ppWS fig1a02

fig1a03 = d [fig1a02, fig1a00]  -- the cat likes which food
fig1a03ws = ppWS fig1a03

fig1a04 = d [fig1a03, lexWS (g1!!1)]  -- C[+wh] the cat likes which food
fig1a04ws = ppWS fig1a04

fig1a05 = d [fig1a04, fig1a01]  -- which food C[+wh] the cat likes which food
fig1a05ws = ppWS fig1a05

fig1a06 = d [lexWS (g1!!10), fig1a05]  -- knows which food C[+wh] the cat likes which food
fig1a06ws = ppWS fig1a06

fig1a07 = d [fig1a06, lexWS (g1!!2)]  -- Jo knows which food C[+wh] the cat likes which food
fig1a07ws = ppWS fig1a07

-- This is the example in Figure 1, with wh movement
fig1a08 = d [lexWS (g1!!0), fig1a07]  -- C Jo knows which food C[+wh] the cat likes which food
fig1a08ws = ppWS fig1a08
fig1a = ppSO ((head.fst) fig1a08)
fig1aEll = ppWS (ell ((head.fst) fig1a08))
fig1aO = ppSO (o_svo ((head.fst) fig1a08))

gxx :: [Lex]
gxx = [
    (["a"], ((["A","Lf"],["C","Lf"]), ("",""))),       -- 0
    (["b"], ((["B","Lf"],["C","Lf"]), ("",""))),       -- 1
    (["a"], ((["C","Rt"],["A","Rt"]), ("",""))),       -- 2
    (["b"], ((["C","Rt"],["B","Rt"]), ("",""))),       -- 3
    (   [], (([],["C","Rt","Lf"]), ("",""))),          -- 4
    (   [], ((["C","Rt","Lf"],["C"]), ("","")))        -- 5
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

-- Figure 2: Head movements (raising and lowering at once)
fig2a =
   S (fromList [
     L ([""], ((["T"],["C"]), ("",""))),
     S (fromList [
       L (["Jo"], (([],["D","K"]), ("",""))),
       S (fromList [
         L (["-s"], ((["v","K"],["T"]), ("",""))),
         S (fromList [
           L (["Jo"], (([],["D","K"]), ("",""))),
           S (fromList [
             L (["-*"], ((["V","D"],["v"]), ("",""))),
             L (["laugh"], (([],["V"]), ("",""))) ]) ]) ]) ]) ])

fig2aSO = ppSO fig2a
fig2aEll = ppWS (ell fig2a)
fig2aOH = ppSO (o_svo (heng fig2a))

fig2bSO = S (fromList [
          S (fromList [
            L (["which"], ((["N"],["D","K","Wh"]), ("",""))),
            L (["food"], (([], ["N"]), ("",""))) ]),
          S (fromList [
            L (["-"], ((["T","Wh"],["C"]), ("",""))),
            S (fromList [
              S (fromList [
                L (["the"], ((["N"],["D","K"]), ("",""))),
                L (["cat"], (([], ["N"]), ("",""))) ]),
              S (fromList [
                L (["-s"], ((["vperf","K"],["T"]), ("",""))),
                S (fromList [
                  L (["have"], ((["perf"],["vperf"]), ("",""))),
                  S (fromList [
                    L (["-en"], ((["vprog"],["perf"]), ("",""))),
                    S (fromList [
                      L (["be"], ((["prog"],["vprog"]), ("",""))),
                    S (fromList [
                      L (["-ing"], ((["v"],["prog"]), ("",""))),
                      S (fromList [
                        S (fromList [
                          L (["the"], ((["N"],["D","K"]), ("",""))),
                          L (["cat"], (([], ["N"]), ("",""))) ]),
                        S (fromList [
                          L (["-"], ((["V","D"],["v"]), ("",""))),
                          S (fromList [
                            S (fromList [
                              L (["which"], ((["N"],["D","K","Wh"]), ("",""))),
                              L (["food"], (([], ["N"]), ("",""))) ]),
                            S (fromList [
                              L (["eat"], ((["D","K"],["V"]), ("",""))),
                              S (fromList [
                                L (["which"], ((["N"],["D","K","Wh"]), ("",""))),
                                L (["food"], (([], ["N"]), ("",""))) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

fig2bEll = ppWS (ell fig2bSO)
fig2bH = ppSO (heng fig2bSO) -- head movement
fig2bOH = ppSO (o_svo (heng fig2bSO)) -- head movement

-- Figure 8 demonstrating multiple occurrences
fig8so :: SO
fig8so = S (fromList [
        L ([], ((["T"],["C"]), ("",""))),
        S (fromList
         [ L (["the man"], (([],["D", "K", "Scr"]), ("",""))),
           S (fromList
             [ L (["the man"], (([],["D", "K", "Scr"]), ("",""))),
             S (fromList
               [ L ([], ((["V", "K", "Scr"],["T"]), ("",""))),
               S (fromList
                 [ L (["the man"], (([],["D", "K", "Scr"]), ("",""))),
                 S (fromList [
                   L (["carefully"], ((["V","Scr"], ["V"]), ("",""))),
                   S (fromList
                     [ L (["the man"], (([],["D", "K", "Scr"]), ("",""))),
                     S (fromList [
                       L (["the man"], (([],["D", "K", "Scr"]), ("",""))),
                       S (fromList [
                         L ([], ((["v","K","D"], ["V"]), ("",""))),
                         S (fromList [
                           L (["praises"], ((["D"], ["v"]), ("",""))),
                           L (["the man"], (([],["D", "K", "Scr"]), ("",""))) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

fig8ws = ppWS (ell fig8so)
fig8O = ppSO (o_svo fig8so)

-- Figure 9 demonstrating remnant movement
fig9so :: SO
fig9so =
 S (fromList [
   L ([""], ((["T"], ["C"]), ("",""))),
   S (fromList [
     L (["I"], (([],["D", "K"]), ("",""))),
     S (fromList [
       L ([""], ((["V","K"], ["T"]), ("",""))),
       S (fromList [
         L (["I"], (([],["D", "K"]), ("",""))),
         S (fromList [
           L (["wonder"], ((["C","D"], ["V"]), ("",""))),
           S (fromList [
             S (fromList [
               L ([""], ((["T","Wh"], ["C"]), ("",""))),
               S (fromList [
                 S (fromList [
                   L (["-s"], ((["V","K"], ["T"]), ("",""))),
                   S (fromList [
                     L (["be"], ((["A"], ["V"]), ("",""))),
                     S (fromList [
                       L (["how"], ((["A"], ["A","Wh"]), ("",""))),
                       S (fromList [
                         L (["likely"], ((["T"],["A"]), ("",""))),
                          S (fromList [
                            L (["to"], ((["V"], ["T"]), ("",""))),
                            S (fromList [
                              L (["win"], ((["D"], ["V"]), ("",""))),
                              L (["John"], (([],["D", "K"]), ("",""))) ]) ]) ]) ]) ]) ]),
               L (["John"], (([],["D", "K"]), ("",""))) ]) ]),
           S (fromList [
             L (["how"], ((["A"], ["A","Wh"]), ("",""))),
             S (fromList [
               L (["likely"], ((["T"],["A"]), ("",""))),
               S (fromList [
                 L (["to"], ((["V"], ["T"]), ("",""))),
                 S (fromList [
                   L (["win"], ((["D"], ["V"]), ("",""))),
                   L (["John"], (([],["D", "K"]), ("",""))) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

fig9ws = ppWS (ell fig9so)
fig9O = ppSO (o_svo fig9so)
fig9HO = ppSO (o_svo (heng fig9so))

-- examples (11a,b,c) showing effect of strong heads in a complement sequence (Z,Y,X)
ex11a :: SO  -- head movement: (x y z, Z)
ex11a = S (fromList [
            L (["w"], ((["Z"],["W"]), ("",""))),
            S (fromList [
              L (["-z*"], ((["Y"],["Z"]), ("",""))),
              S (fromList [
                L (["-y"], ((["X"],["Y"]), ("",""))),
                S (fromList [
                  L (["x"], ((["V"],["X"]), ("",""))),
                  S (fromList [
                    L (["v"], ((["XX"],["V"]), ("",""))),
                    L (["..."], (([],["XX"]), ("","")))
                  ]) ]) ]) ]) ])

ex11aSO = ppSO ex11a
ex11aEll = ppWS (ell ex11a)
ex11aH = ppSO (heng ex11a) -- head movement
ex11aOH = ppSO (o_svo (heng ex11a)) -- head movement

ex11b :: SO  -- head movement: (x y z, Y)
ex11b = S (fromList [
            L (["-z"], ((["Y"],["Z"]), ("",""))),
            S (fromList [
              L (["-y*"], ((["X"],["Y"]), ("",""))),
              S (fromList [
                L (["x"], ((["W"],["X"]), ("",""))),
                L (["..."], (([],["W"]), ("","")))
                ]) ]) ])

ex11bSO = ppSO ex11b
ex11bEll = ppWS (ell ex11b)
ex11bH = ppSO (heng ex11b) -- head movement
ex11bOH = ppSO (o_svo (heng ex11b)) -- head movement

ex11c :: SO
ex11c = S (fromList [
            L (["-z"], ((["Y"],["Z"]), ("",""))),
            S (fromList [
              L (["-y"], ((["X"],["Y"]), ("",""))),
              S (fromList [
                L (["x*"], ((["W"],["X"]), ("",""))),
                L (["..."], (([],["W"]), ("","")))
                ]) ]) ])

ex11cSO = ppSO ex11c
ex11cEll = ppWS (ell ex11c)
ex11cH = ppSO (heng ex11c)
ex11cOH = ppSO (o_svo (heng ex11c))

ex11x :: SO  -- should be the same as ex11aSO: (x y z, Z)
ex11x = S (fromList [
            L (["-z*"], ((["Y"],["Z"]), ("",""))),
            S (fromList [
              L (["-y"], ((["X"],["Y"]), ("",""))),
              S (fromList [
                L (["x*"], ((["W"],["X"]), ("",""))),
                L (["..."], (([],["W"]), ("","")))
                ]) ]) ])

ex11xSO = ppSO ex11x
ex11xEll = ppWS (ell ex11x)
ex11xH = ppSO (heng ex11x)
ex11xOH = ppSO (o_svo (heng ex11x))

--  fig7 = fig1 but with separate affixes and affix hopping
fig7 =
   S (fromList [
     L ([""], ((["T"],["C"]), ("",""))),
     S (fromList [
       L (["Jo"], (([],["D","K"]), ("",""))),
       S (fromList [
         L (["-s"], ((["v","K"],["T"]), ("",""))),
         S (fromList [
           L (["Jo"], (([],["D","K"]), ("",""))),
           S (fromList [
             L (["-"], ((["V","D"],["v"]), ("",""))),
             S (fromList [
               L (["know"], ((["C"],["V"]), ("",""))),
               S (fromList [
                 S (fromList [
                   L (["which"], ((["N"],["D","K","Wh"]), ("",""))),
                   L (["food"], (([],["N"]), ("",""))) ]),
                 S (fromList [
                   L ([""], ((["T","Wh"],["C"]), ("",""))),
                   S (fromList [
                     S (fromList [
                       L (["the"], ((["N"],["D","K"]), ("",""))),
                       L (["cat"], (([],["N"]), ("",""))) ]),
                     S (fromList [
                       L (["-s"], ((["v","K"],["T"]), ("",""))),
                       S (fromList [
                         S (fromList [
                           L (["the"], ((["N"],["D","K"]), ("",""))),
                           L (["cat"], (([],["N"]), ("",""))) ]),
                         S (fromList [
                           L (["-"], ((["V","D"],["v"]), ("",""))),
                           S (fromList [
                             S (fromList [
                               L (["which"], ((["N"],["D","K","Wh"]), ("",""))),
                               L (["food"], (([],["N"]), ("",""))) ]),
                             S (fromList [
                               L (["like"], ((["D","K"],["V"]), ("",""))),
                               S (fromList [
                                 L (["which"], ((["N"],["D","K","Wh"]), ("",""))),
                                 L (["food"], (([],["N"]), ("",""))) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])
fig7SO = ppSO fig7
fig7Ell = ppWS (ell fig7)
fig7OH = ppSO (o_svo (heng fig7))

-- Figure 3
ex12a :: SO
ex12a =
   S (fromList [
     L ([""], ((["T"],["C"]), ("",""))),
     S (fromList [
       L (["he"], (([],["D","K"]), ("",""))),
       S (fromList [
         L (["-s"], ((["Sigma","K"],["T"]), ("",""))),
         S (fromList [
           L (["not"], (([],["neg"]), ("",""))),
           S (fromList [
             L (["-"], ((["v","neg"],["Sigma"]), ("",""))),
             S (fromList [
               L (["he"], (([],["D","K"]), ("",""))),
               S (fromList [
                 L (["-*"], ((["V","D"],["v"]), ("",""))),
                 L (["laugh"], (([],["V"]), ("",""))) ]) ]) ]) ]) ]) ]) ])

ex12SO = ppSO ex12a
ex12Ell = ppWS (ell ex12a)
ex12aOH = ppSO (o_svo (heng ex12a))

ex12b :: SO
ex12b = S (fromList [
         L (["who"], (([],["D","K","Wh"]), ("",""))),
         S (fromList [
           L (["-*"], ((["T","Wh"],["C"]), ("",""))),
           S (fromList [
             L (["he"], (([],["D","K"]), ("",""))),
             S (fromList [
               L (["-s"], ((["v","K"],["T"]), ("",""))),
                 S (fromList [
                   L (["he"], (([],["D","K"]), ("",""))),
                   S (fromList [
                     L (["-*"], ((["V","D"],["v"]), ("",""))),
                     S (fromList [
                       L (["who"], (([],["D","K","Wh"]), ("",""))),
                       S (fromList [
                         L (["see"], ((["D","K"],["V"]), ("",""))),
                         L (["who"], (([],["D","K","Wh"]), ("",""))) ]) ]) ]) ]) ]) ]) ]) ])


ex12bSO = ppSO ex12b
ex12bEll = ppWS (ell ex12b)
ex12bOH = ppSO (o_svo (heng ex12b))

ex12c :: SO
ex12c =
   S (fromList [
     L ([""], ((["T"],["C"]), ("",""))),
     S (fromList [
       L (["he"], (([],["D","K"]), ("",""))),
       S (fromList [
         L (["-s"], ((["Sigma","K"],["T"]), ("",""))),
         S (fromList [
           L (["\""], (([],["foc"]), ("",""))),
           S (fromList [
             L (["-"], ((["v","foc"],["Sigma"]), ("",""))),
             S (fromList [
               L (["he"], (([],["D","K"]), ("",""))),
               S (fromList [
                 L (["-*"], ((["V","D"],["v"]), ("",""))),
                 L (["laugh"], (([],["V"]), ("",""))) ]) ]) ]) ]) ]) ]) ])

ex12cSO = ppSO ex12c
ex12cEll = ppWS (ell ex12c)
ex12cOH = ppSO (o_svo (heng ex12c))

-- Javanese-like multiple head movement: z y x
ex11d :: SO
ex11d = S (fromList [
            L (["--z*"], ((["Y"],["Z"]), ("",""))),
            S (fromList [
              L (["y"], ((["X"],["Y"]), ("",""))),
              S (fromList [
                L (["x*"], ((["W"],["X"]), ("",""))),
                L (["..."], (([],["W"]), ("","")))
                ]) ]) ])

ex11dSO = ppSO ex11d
ex11dEll = ppWS (ell ex11d)
ex11dH = ppSO (heng ex11d)
ex11dOH = ppSO (o_svo (heng ex11d))
--ex11dHj = ppSO (hjava ex11d)
--ex11dOHj = ppSO (o_svo (hjava ex11d))

fig4 :: SO
fig4 = S (fromList [
           L (["---*"], ((["T"],["C"]), ("",""))),
           S (fromList [
             L (["Tono"], (([],["D","K"]), ("",""))),
             S (fromList [
               L ([""], ((["Aux","K"],["T"]), ("",""))),
                 S (fromList [
                 L (["want"], ((["v"], ["Aux"]), ("",""))),
                   S (fromList [
                     L (["Tono"], (([],["D","K"]), ("",""))),
                     S (fromList [
                       L (["can"], ((["V","D"],["v"]), ("",""))),
                       S (fromList [
                         L (["speak"], ((["D"],["V"]), ("",""))),
                         L (["English"], (([],["D"]), ("",""))) ]) ]) ]) ]) ]) ]) ])

fig4SO = ppSO fig4
fig4Ell = ppWS (ell fig4)
fig4H = ppSO (heng fig4) -- head movement
fig4OH = ppSO (o_svo (heng fig4)) -- head movement
--fig4Hj = ppSO (hjava fig4) -- head movement
--fig4OHj = ppSO (o_svo (hjava fig4)) -- head movement
