-- https://github.com/epstabler/mgt/tree/main/haskell/Mg/MgH.hs
module MgH where  -- Multiset needed. E.g., start ghci with: stack ghci --package multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List (partition)
import Mg
import MgL

-- map strings [w,...] to number of head-incorporator +'s on w, else 0
inc :: [String] -> Int
inc s = case s of { (('+':s'):_) -> 1 + inc [s'] ; _ -> 0 }

-- where i = #heads needed by c-commanding selector, (h i so) = (heads, so')
h :: Int -> SO -> ([String], SO)
h 0 (L lex) = ([], L lex)
h 1 (L (w,fs)) = (w, L ([],fs))
h i (S s) =
  let ([nws],pws:pwss) = partition ((/= []).fst.head.snd) (map ell (MultiSet.toList s)) in -- partition neg WS
    case (head.fst) nws of
      L (w,fs) ->
        if isCoord (L (w,fs))
        then 
          let (hs,pso) = h i ((head.fst) pws) in
          let psos = atbh i hs pwss in
            if i == 0
            then (hs, S (MultiSet.fromList (L (w, fs) : pso : psos)))
            else
              if i == 1
              then (hs, S (MultiSet.fromList (L (w, fs) : pso : psos)))
              else (hs, S (MultiSet.fromList (L (w, fs) : pso : psos)))
        else
          let i' = inc w + max 0 (i-1) in
          let (hs,pso) = h i' ((head.fst) pws) in
          let psos = atbh i' hs pwss in
            if i == 0
            then ([], S (MultiSet.fromList (L (hs ++ w, fs) : pso : psos)))
            else
              if i == 1
              then (hs ++ w, S (MultiSet.fromList (L ([], fs) : pso : psos)))
              else (w ++ hs, S (MultiSet.fromList (L ([], fs) : pso : psos)))
      nso ->
        if isCoord nso
        then
          let (hs,nso') = h i nso in
          let (hs,pso) = h i ((head.fst) pws) in
          let psos = atbh i hs pwss in
            if i == 0
            then ([], S (MultiSet.fromList (nso' : pso : psos)))
            else
              if i == 1
              then (hs, S (MultiSet.fromList (nso' : pso : psos)))
              else (hs, S (MultiSet.fromList (nso' : pso : psos)))
        else
          let (hs,nso') = h i nso in
          let psos = map (head.fst) (pws:pwss) in
            (hs, S (MultiSet.fromList (nso' : psos)))
  where
    isCoord :: SO -> Bool
    isCoord (S s) = let ([(so:_,_)],_) = partition ((/= []).fst.head.snd) (map ell (MultiSet.toList s)) in isCoord so
    isCoord (L (w, (_:(Plus _):_,_))) = True    -- catch coord case with plus
    isCoord _ = False

    atbh :: Int -> [String] -> [WS] -> [SO]  -- collect additional comps with same hs extracted
    atbh _ _ [] = []
    atbh i hs (pws:pwss) = let (hs', pso) = h i ((head.fst) pws) in
        if hs' == hs then pso:atbh i hs pwss else error "atbh error"
