-- https://github.com/epstabler/mgt/tree/main/haskell/MgBin/MgBinH.hs
module MgBinH where -- "H" for "Head movement"
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (partition)
import MgBin
import MgBinL

-- map strings [w,...] to number of head-incorporator +'s on w, else 0
inc :: [String] -> Int
inc s = case s of { (('+':s'):_) -> 1 + inc [s'] ; _ -> 0 }

-- (h i so) = (heads, so') where i = #heads needed by c-commanding selector
h :: Int -> SO -> ([String], SO)
h 0 (L lex) = ([], L lex)
h i (S s) = let ([nws],[pws]) = partition ((/= []).fst.head.snd) (map ell (Set.toList s)) in -- partition neg WS
    case (head.fst) nws of 
      L (w,fs) -> let (hs,pso) = h (inc w) ((head.fst) pws) in
        if i == 0
        then ([], S (Set.fromList (L (hs ++ w, fs) : pso : [])))
        else (hs ++ w, S (Set.fromList (L ([], fs) : pso : [])))
      nso ->
        let (hs,nso') = h i nso in
        let pso = (head.fst) pws in
          (hs, S (Set.fromList [nso',pso]))
