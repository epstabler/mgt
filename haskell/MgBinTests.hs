
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
label2str ([],[]) = "T"
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

-- example grammar from section 1.1.2
g112 :: [Lex]
g112 = [
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

ex00 = ppMg g112

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

ex0301 = S (fromList [
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

ex0302 = ppSO ex0301
ex0303 = ppWS (ell ex0301)
ex0304 = ppSO (h ex0301) -- head movement
ex0304a = ppSO (o_svo (h ex0301)) -- head movement

-- Example of Figure 2, demonstrating multiple occurrences
ex08 :: SO
ex08 = S (fromList [
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

ex08a = ppWS (ell ex08)
ex08b = ppSO (o_svo ex08)

-- this example is from Figure 3
ex09 :: SO
ex09 =
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

ex09a = ppWS (ell ex09)
ex09b = ppSO (o_svo ex09)

-- examples showing effect of strong heads in a complement sequence (Z,Y,X)
ex1101a :: SO  -- head movement: (x y z, Z)
ex1101a = S (fromList [
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

ex1101aa = ppSO ex1101a
ex1101ab = ppWS (ell ex1101a)
ex1101ac = ppSO (h ex1101a) -- head movement
ex1101ad = ppSO (o_svo (h ex1101a)) -- head movement

ex1101b :: SO  -- head movement: (x y z, Y)
ex1101b = S (fromList [
            L (["-z"], ((["Y"],["Z"]), ("",""))),
            S (fromList [
              L (["-y*"], ((["X"],["Y"]), ("",""))),
              S (fromList [
                L (["x"], ((["W"],["X"]), ("",""))),
                L (["..."], (([],["W"]), ("","")))
                ]) ]) ])

ex1101ba = ppSO ex1101b
ex1101bb = ppWS (ell ex1101b)
ex1101bc = ppSO (h ex1101b) -- head movement
ex1101bd = ppSO (o_svo (h ex1101b)) -- head movement

ex1101c :: SO  -- should be the same as ex1101a: (x y z, X)
ex1101c = S (fromList [
            L (["-z"], ((["Y"],["Z"]), ("",""))),
            S (fromList [
              L (["-y"], ((["X"],["Y"]), ("",""))),
              S (fromList [
                L (["x*"], ((["W"],["X"]), ("",""))),
                L (["..."], (([],["W"]), ("","")))
                ]) ]) ])

ex1101ca = ppSO ex1101c
ex1101cb = ppWS (ell ex1101c)
ex1101cc = ppSO (h ex1101c)
ex1101cd = ppSO (o_svo (h ex1101c))

ex1101d :: SO  -- should be the same as ex1101a: (x y z, Z)
ex1101d = S (fromList [
            L (["-z*"], ((["Y"],["Z"]), ("",""))),
            S (fromList [
              L (["-y"], ((["X"],["Y"]), ("",""))),
              S (fromList [
                L (["x*"], ((["W"],["X"]), ("",""))),
                L (["..."], (([],["W"]), ("","")))
                ]) ]) ])

ex1101da = ppSO ex1101d
ex1101db = ppWS (ell ex1101d)
ex1101dc = ppSO (h ex1101d)
ex1101dd = ppSO (o_svo (h ex1101d))

ex1101e :: SO -- multiple head movement "tucking in", so: (z x y, X)
ex1101e = S (fromList [
            L (["--z*"], ((["Y"],["Z"]), ("",""))),
            S (fromList [
              L (["y"], ((["X"],["Y"]), ("",""))),
              S (fromList [
                L (["x*"], ((["W"],["X"]), ("",""))),
                L (["..."], (([],["W"]), ("","")))
                ]) ]) ])

ex1101ea = ppSO ex1101e
ex1101eb = ppWS (ell ex1101e)
ex1101ec = ppSO (h ex1101e)
ex1101ed = ppSO (o_svo (h ex1101e))

--  head movement with affix hopping
ex29 =
   S (fromList [
     L ([""], ((["T"],["C"]), ("",""))),
     S (fromList [
       L (["he"], (([],["D","K"]), ("",""))),
       S (fromList [
         L (["-s"], ((["v","K"],["T"]), ("",""))),
         S (fromList [
           L (["he"], (([],["D","K"]), ("",""))),
           S (fromList [
             L (["-"], ((["V","D"],["v"]), ("",""))),
             S (fromList [
               L (["know"], ((["C"],["V"]), ("",""))),
               S (fromList [
                 S (fromList [
                   L (["which"], ((["N"],["D","K","Wh"]), ("",""))),
                   L (["wine"], (([],["N"]), ("",""))) ]),
                 S (fromList [
                   L ([""], ((["T","Wh"],["C"]), ("",""))),
                   S (fromList [
                     S (fromList [
                       L (["the"], ((["N"],["D","K"]), ("",""))),
                       L (["king"], (([],["N"]), ("",""))) ]),
                     S (fromList [
                       L (["-s"], ((["v","K"],["T"]), ("",""))),
                       S (fromList [
                         S (fromList [
                           L (["the"], ((["N"],["D","K"]), ("",""))),
                           L (["king"], (([],["N"]), ("",""))) ]),
                         S (fromList [
                           L (["-"], ((["V","D"],["v"]), ("",""))),
                           S (fromList [
                             S (fromList [
                               L (["which"], ((["N"],["D","K","Wh"]), ("",""))),
                               L (["wine"], (([],["N"]), ("",""))) ]),
                             S (fromList [
                               L (["like"], ((["D","K"],["V"]), ("",""))),
                               S (fromList [
                                 L (["which"], ((["N"],["D","K","Wh"]), ("",""))),
                                 L (["wine"], (([],["N"]), ("",""))) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])
ex29a = ppSO ex29
ex29b = ppWS (ell ex29)
ex29c = ppSO (o_svo (h ex29))

ex30 =
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

ex30a = ppSO ex30
ex30b = ppWS (ell ex30)
ex30c = ppSO (o_svo (h ex30))

ex12a :: SO
ex12a =
   S (fromList [
     L ([""], ((["T"],["C"]), ("",""))),
     S (fromList [
       L (["Jo"], (([],["D","K"]), ("",""))),
       S (fromList [
         L (["-s"], ((["Sigma","K"],["T"]), ("",""))),
         S (fromList [
           L (["not"], (([],["neg"]), ("",""))),
           S (fromList [
             L (["-*"], ((["v","neg"],["Sigma"]), ("",""))),
             S (fromList [
               L (["Jo"], (([],["D","K"]), ("",""))),
               S (fromList [
                 L (["-"], ((["V","D"],["v"]), ("",""))),
                 L (["laugh"], (([],["V"]), ("",""))) ]) ]) ]) ]) ]) ]) ])

ex12aa = ppSO ex12a
ex12ab = ppWS (ell ex12a)
ex12ac = ppSO (o_svo (h ex12a))

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


ex12ba = ppSO ex12b
ex12bb = ppWS (ell ex12b)
ex12bc = ppSO (o_svo (h ex12b))

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

ex12ca = ppSO ex12c
ex12cb = ppWS (ell ex12c)
ex12cc = ppSO (o_svo (h ex12c))

-- Javanese-like multiple head movement
ex1801 :: SO
ex1801 = S (fromList [
           L (["--"], ((["Vgelem"],["C"]), ("",""))),
           S (fromList [
             L (["Tono"], (([],["D"]), ("",""))),
             S (fromList [
               L (["want"], ((["Visa","D"], ["Vgelem"]), ("",""))),
               S (fromList [
                 L (["can"], ((["V"],["Visa"]), ("",""))),
                 S (fromList [
                   L (["speak"], ((["D"],["V"]), ("",""))),
                   L (["English"], (([],["D"]), ("",""))) ]) ]) ]) ]) ])

ex1802 = ppSO ex1801
ex1803 = ppWS (ell ex1801)
ex1804 = ppSO (h ex1801) -- head movement
ex1804a = ppSO (o_svo (h ex1801)) -- head movement

