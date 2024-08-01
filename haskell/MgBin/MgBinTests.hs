-- https://github.com/epstabler/mgt/tree/main/haskell/MgBin/MgBinTests.hs
module MgBinTests where
import Data.Set (Set)
import qualified Data.Set as Set
import MgBin
import MgBinL
import MgBinH
import MgBinO

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
label2str ([],p) = (joinstr "." (map show p))
label2str ( n,p) = (joinstr "." (map show n)) ++ " -o " ++ (joinstr "." (map show p))

-- convert lexical item to pretty string
lex2str ([],f) = "([], " ++ (label2str f) ++ ")"
lex2str (s,f) = "(" ++ joinstr " " s ++ ", " ++ (label2str f) ++ ")"

-- pretty print grammar
ppMg = mapM_ (putStrLn.lex2str)

-- convert SO to pretty string
so2str (S so) = "{" ++ (joinstr ", " (map so2str (Set.elems so))) ++ "}"
so2str (L (w,label)) = "(" ++ (joinstr " " w) ++ ", " ++ (label2str label) ++ ")"

-- pretty print SO
ppSO so = do { ppSO' 0 so ; putStrLn "" }

-- pretty print SO with indent i
ppSO' i (L w) = do { putStr (so2str (L w)) }
ppSO' i (S so) = do { putStr "{ " ; ppSOs (i+2) (Set.elems so) ; putStr " }" }
ppSO' i (O t) = do { putStr "[ " ; ppPh' (i+2) t ; putStr " ]" }

-- pretty print a list of SOs with indent i
ppSOs i [] = putStr ""
ppSOs i (x:[]) = do { ppSO' i x }
ppSOs i (x:xs) = do { ppSO' i x ; putStrLn "," ; tab i ; ppSOs i xs }

-- pretty print a workspace
ppWS ([],[]) = putStrLn ""
ppWS (so:sos, label:labels) = do { ppSO so ; putStrLn (label2str label); ppWS (sos,labels) }

-- pretty print PhTree
ppPh t = do { ppPh' 0 t ; putStrLn "" }

-- pretty print PhTree with indent i
ppPh' i (Pl w) = do { putStr (so2str (L w)) }
ppPh' i (Ps t) = do { putStr "[ " ; ppPhs (i+2) t ; putStr " ]" }

-- pretty print a list of PhTrees with indent i
ppPhs i [] = putStr ""
ppPhs i (x:[]) = do { ppPh' i x }
ppPhs i (x:xs) = do { ppPh' i x ; putStrLn "," ; tab i ; ppPhs i xs }

-- example grammar from section 1.1.2
g112 :: [Lex]
g112 = [
    ([], ([V],[C])),             -- 0
    ([], ([V,Wh],[C])),          -- 1
    (["Jo"], ([],[D])),          -- 2
    (["the"], ([N],[D])),        -- 3
    (["which"], ([N],[D,Wh])),   -- 4
    (["who"], ([],[D,Wh])),      -- 5
    (["cat"], ([],[N])),         -- 6
    (["dog"], ([],[N])),         -- 7
    (["food"], ([],[N])),        -- 8
    (["likes"],([D,D],[V])),     -- 9
    (["knows"], ([C,D],[V]))     -- 10
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

ex0010 = d [ex008]  -- which food C[+wh] the cat likes which food
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
    (["a"], ([A,Lf],[C,Lf])),       -- 0
    (["b"], ([B,Lf],[C,Lf])),       -- 1
    (["a"], ([C,Rt],[A,Rt])),       -- 2
    (["b"], ([C,Rt],[B,Rt])),       -- 3
    (   [], ([],[C,Rt,Lf])),        -- 4
    (   [], ([C,Rt,Lf],[C]))        -- 5
    ]

ex01 = ppMg gxx

-- deriving complete aa from gxx requires 7 merges, with remnant movement
ex0100 = d [lexWS (gxx!!2), lexWS (gxx!!4)]  -- a
ex0101 = ppWS ex0100
ex0102 = d [ex0100]
ex0103 = ppWS ex0102
ex0104 = d [lexWS (gxx!!0), ex0102]   -- a
ex0105 = ppWS ex0104
ex0106 = d [ex0104]
ex0107 = ppWS ex0106 -- 
ex0108 = d [lexWS (gxx!!5), ex0106]
ex0109 = ppWS ex0108
ex0110 = d [ex0108]
ex0111 = ppWS ex0110
ex0112 = d [ex0110]
ex0113 = ppWS ex0112
ex0113a = ppSO (o_svo ((head.fst) ex0112))

-- deriving complete abab, we continue from ex0106
ex0114 = d [lexWS (gxx!!3), ex0106]   -- b a a
ex0115 = ppWS ex0114
ex0116 = d [ex0114]
ex0117 = ppWS ex0116
ex0118 = d [lexWS (gxx!!1), ex0116]   -- b b a a
ex0119 = ppWS ex0118
ex0120 = d [ex0118]
ex0121 = ppWS ex0120 -- 
ex0122 = d [lexWS (gxx!!5), ex0120]
ex0123 = ppWS ex0122
ex0124 = d [ex0122]
ex0125 = ppWS ex0124
ex0126 = d [ex0124]
ex0127 = ppWS ex0126

ex0128 = ppSO ((head.fst) ex0126)
ex0128a = ppSO (o_svo ((head.fst) ex0126))
ex0129 = ppWS (ell ((head.fst) ex0126))

-- examples from \S1.3.3 of the paper: replicating Stabler (2001: \S2.1)
g133 :: [Lex]
g133 = [
    ([], ([T],[C])),
    ([], ([T,Wh],[C])),

    (["-s"], ([Modal,K],[T])),
    (["-s"], ([Have,K],[T])),
    (["-s"], ([Be,K],[T])),
    (["-s"], ([Vx,K],[T])),

    (["will"], ([Have],[Modal])),
    (["will"], ([Be],[Modal])),
    (["will"], ([Vx],[Modal])),

    (["have"], ([Been],[Have])),
    (["have"], ([Ven],[Have])),

    (["be"], ([Ving],[Be])),
    (["been"], ([Ving],[Been])),

    ([], ([V,D],[Vx])),
    (["-en"], ([V,D],[Ven])),
    (["-ing"], ([V,D],[Ving])),

    (["eat"], ([D,K],[V])),
    (["laugh"], ([],[V])),

    (["the"], ([N],[D,K])),
    (["which"], ([N],[D,K,Wh])),

    (["king"], ([],[N])),
    (["pie"], ([],[N]))
    ]

ex02 = ppMg g133

ex0201 :: SO
ex0201 = S (Set.fromList [
          S (Set.fromList [
            L (["which"], ([N],[D,K,Wh])),
            L (["pie"], ([], [N])) ]),
          S (Set.fromList [
            L (["+"], ([T,Wh],[C])),
            S (Set.fromList [
              S (Set.fromList [
                L (["the"], ([N],[D,K])),
                L (["king"], ([], [N])) ]),
              S (Set.fromList [
                L (["+s"], ([Have,K],[T])),
                S (Set.fromList [
                  L (["have"], ([Been],[Have])),
                  S (Set.fromList [
                    L (["been"], ([Ving],[Been])),
                    S (Set.fromList [
                      S (Set.fromList [
                        L (["the"], ([N],[D,K])),
                        L (["king"], ([], [N])) ]),
                      S (Set.fromList [
                        L (["+ing"], ([V,D],[Ving])),
                        S (Set.fromList [
                          S (Set.fromList [
                             L (["which"], ([N],[D,K,Wh])),
                             L (["pie"], ([], [N])) ]),
                          S (Set.fromList [
                            L (["eat"], ([D,K],[V])),
                            S (Set.fromList [
                               L (["which"], ([N],[D,K,Wh])),
                               L (["pie"], ([], [N])) ]) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

ex0202 = ppSO ex0201
ex0203 = ppWS (ell ex0201)
ex0204 = ppSO (snd (h 0 ex0201)) -- head movement
ex0204a = ppSO (o_svo (snd (h 0 ex0201))) -- head movement
ex0204b = ppSO (o_sov (snd (h 0 ex0201))) -- head movement

-- NOT WORKING YET
ex28 :: SO
ex28 = S (Set.fromList [
         L (["who"], ([],[D,K,Wh])),
         S (Set.fromList [
           L (["+"], ([T,Wh],[C])),
           S (Set.fromList [
             S (Set.fromList [
               L (["Jack"], ([],[D,K])),
               S (Set.fromList [
                 L (["+s"], ([Do,K],[T])),
                   S (Set.fromList [
                     L (["Jack"], ([],[D,K])),
                     S (Set.fromList [
                       L (["do"], ([V,D],[Do])),
                       S (Set.fromList [
                         L (["who"], ([],[D,K,Wh])),
                         S (Set.fromList [
                           L (["praise"], ([D,K],[V])),
                           L (["who"], ([],[D,K,Wh])) ]) ]) ]) ]) ]) ]),
             S (Set.fromList [
                L (["and"], ([T,T],[T])),
                S (Set.fromList [
                  L (["Mary"], ([],[D,K])),
                  S (Set.fromList [
                    L (["+s"], ([Do,K],[T])),
                      S (Set.fromList [
                        L (["Mary"], ([],[D,K])),
                        S (Set.fromList [
                          L (["do"], ([V,D],[Do])),
                          S (Set.fromList [
                            L (["who"], ([],[D,K,Wh])),
                            S (Set.fromList [
                              L (["ignore"], ([D,K],[V])),
                              L (["who"], ([],[D,K,Wh])) ]) ]) ]) ]) ]) ]) ]) ]) ]) ])

ex28a = ppWS (ell ex28)
ex28b = ppSO (snd (h 0 ex28))
ex28c = ppSO (o_svo (snd (h 0 ex28)))
ex28d = ppSO (o_sov (snd (h 0 ex28)))
