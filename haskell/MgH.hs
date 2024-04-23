module MgH where  -- Multiset needed. E.g., start ghci with: stack ghci --package multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List
import qualified Data.List as List

import Mg
import MgL
import MgO

-- where i = #heads needed by selector above, (h i so) = (heads, so')
h :: Int -> SO -> ([String], SO)
h 0 (L lex) = ([], L lex)
h i (S s) = case List.partition negWS (map ell (MultiSet.toList s)) of
  ([nws],pws:pwss) ->
    case (fst.maxx.(MultiSet.toList)) nws of
      L (w,fs) -> let i' = inc w in 
        let (hs,pso) = h i' ((fst.maxx.(MultiSet.toList)) pws) in
        let psos = atbh i' hs pwss in
          if i == 0
          then ([], S (MultiSet.fromList (L (hs ++ w, fs) : pso : psos)))
          else (hs ++ w, S (MultiSet.fromList (L ([], fs) : pso : psos)))
      nso ->
        let (hs,nso') = h i nso in
        let psos = (map (fst.maxx.(MultiSet.toList)) (pws:pwss)) in
          (hs, S (MultiSet.fromList (nso' : psos)))

atbh :: Int -> [String] -> [WS] -> [SO]  -- collect comps with hs extracted
atbh _ _ [] = []
atbh i hs (pws:pwss) = case h i ((fst.maxx.(MultiSet.toList)) pws) of
     (hs', pso) -> if hs' == hs then pso:(atbh i hs pwss) else (error "atbh error")

inc :: [String] -> Int -- map [w] to # head-incorporator +'s on w, else 0
inc [] = 0
inc (w:_) = cc w where
    cc ('+' : s) = 1 + (cc s)
    cc _ = 0
