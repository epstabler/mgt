module MgBinTests where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import qualified Data.List as List

import MgBin
import MgBinTransduction
import MgBinLinearization

{--- IO: prettyPrint various structures ---}
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

-- pretty print PhTree
ppPh t = do { ppPh' 0 t ; putStrLn "" }

-- pretty print PhTree with indent i
ppPh' i (Pl w) = do { putStr (so2str (L w)) }
ppPh' i (Ps t) = do { putStr "[ " ; ppPhs (i+2) t ; putStr " ]" }

-- pretty print a list of PhTrees with indent i
ppPhs i [] = putStr ""
ppPhs i (x:[]) = do { ppPh' i x }
ppPhs i (x:xs) = do { ppPh' i x ; putStrLn "," ; tab i ; ppPhs i xs }

-- pretty print LSO
ppLSO lso = do { ppLSO' 0 lso ; putStrLn "" }

-- pretty print LSO with indent i
ppLSO' i ((L w),label) = do { putStr (so2str (L w)) ; putStr ","; putStr (label2str label) }
ppLSO' i ((S so),label) = do { putStr "{ " ; ppSOs (i+2) (Set.elems so) ; putStr " },"; putStr (label2str label) }
ppLSO' i ((O t),label) = do { ppPh t; putStr (label2str label) }

-- pretty print a list of SOs with indent i
ppLSOs i [] = putStr ""
ppLSOs i (x:[]) = do { ppLSO' i x }
ppLSOs i (x:xs) = do { ppLSO' i x ; putStrLn "," ; tab i ; ppLSOs i xs }

-- pretty print a workspace with indent i
ppWS ws = do {ppLSOs 0 (Set.toList ws); putStrLn "" }

-- example grammar from section 1.1.2
g112 :: [Lex]
g112 = [
    ([], ([V],[C])),
    ([], ([V,Wh],[C])),
    (["Jo"], ([],[D])),
    (["the"], ([N],[D])),
    (["which"], ([N],[D,Wh])),
    (["who"], ([],[D,Wh])),
    (["cat"], ([],[N])),
    (["dog"], ([],[N])),
    (["food"], ([],[N])),
    (["likes"],([D,D],[V])),
    (["knows"], ([C,D],[V]))
    ]

-- create lexical LSO
lexLSO :: SO -> LSO
lexLSO (L x) = (L x, snd x)

-- create lexical WS
lexWS :: SO -> WS
lexWS x = Set.singleton (lexLSO x)

ex00 = ppMg g112

exA = S (Set.fromList [
            L (["likes"],([D,D],[V])),
              L (["who"], ([],[D,Wh])) ])

exB = S (Set.fromList [
             L (["Jo"], ([],[D])),
             exA ])

exC = S (Set.fromList [
            L ([], ([V,Wh],[C])),
            exB ])

ex01 :: SO
ex01 = S (Set.fromList
         [ L ([], ([V,Wh],[C])),
           S (Set.fromList
              [ L (["Jo"], ([],[D])),
                S (Set.fromList [
                      L (["likes"],([D,D],[V])),
                      L (["who"], ([],[D,Wh])) ])
              ])
         ])
ex01a = ppWS (ell ex01)

ex02 :: SO
ex02 = S (Set.fromList [
         L (["which"], ([N],[D,Wh])),
         L (["food"], ([],[N])) ])

ex03 :: SO
ex03 = S (Set.fromList [
         L ([], ([V,Wh],[C])),
         S (Set.fromList [
           S (Set.fromList [
             L (["the"], ([N],[D])),
             L (["cat"], ([],[N])) ]),
           S (Set.fromList [
             L (["likes"],([D,D],[V])),
             ex02 ]) ]) ])

ex04 :: SO
ex04 = S (Set.fromList [
             ex02,
             ex03 ])

ex05 :: SO
ex05 = S (Set.fromList [
             L (["knows"], ([C,D],[V])),
             ex04 ])

ex06 :: SO
ex06 = S (Set.fromList [
             L (["Jo"], ([],[D])),
             ex05 ])

-- This is the example in Figure 1
ex07 ::SO
ex07 = S (Set.fromList [
             L ([], ([V],[C])),
             ex06 ])

ex07a = ppSO ex07
ex07b = ppWS (ell ex07)
ex07c = ppSO (ord_svo ex07)
ex07d = ppSO (ord_sov ex07)
