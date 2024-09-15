module MgH where
import Data.MultiSet (MultiSet, fromList, toList) -- Multiset needed. E.g., start ghci with: stack ghci multiset
import Data.List (partition)
import Mg

-- map strings [w,...] to number of head-incorporator +'s on w, else 0
inc :: [String] -> Int
inc s = case s of { (('+':s'):_) -> 1 + inc [s'] ; _ -> 0 }

-- where i = #heads needed by c-commanding selector, (h i so) = (heads, so')
h :: Int -> SO -> ([String], SO)
h 0 (L lex) = ([], L lex)
h 1 (L (w,fs)) = (w, L ([],fs))
h i (S s) =
  let ([nws],pws:pwss) = partition ((/= []).fst.head.snd) (map ell (toList s)) in -- partition neg WS
    case (head.fst) nws of
      L (w,fs) ->
        if isCoord (L (w,fs))
        then 
          let (hs,pso) = h i ((head.fst) pws) in
          let psos = atbh i hs pwss in 
            (hs, S (fromList (L (w, fs) : pso : psos)))
        else
          let i' = inc w + max 0 (i-1) in
          let (hs,pso) = h i' ((head.fst) pws) in
          let psos = atbh i' hs pwss in case i of
            0 -> ([], S (fromList (L (hs ++ w, fs) : pso : psos)))
            1 -> (hs ++ w, S (fromList (L ([], fs) : pso : psos)))
            _ -> (w ++ hs, S (fromList (L ([], fs) : pso : psos)))
      nso ->
        if isCoord nso
        then
          let (hs,nso') = h i nso in
          let (hs,pso) = h i ((head.fst) pws) in
          let psos = atbh i hs pwss in case i of
            0 -> ([], S (fromList (nso' : pso : psos)))
            _ -> (hs, S (fromList (nso' : pso : psos)))
        else
          let (hs,nso') = h i nso in
          let psos = map (head.fst) (pws:pwss) in
            (hs, S (fromList (nso' : psos)))
  where
    isCoord :: SO -> Bool
    isCoord (S s) = let ([(so:_,_)],_) = partition ((/= []).fst.head.snd) (map ell (toList s)) in isCoord so
    isCoord (L (w, (_:f:_,_))) = snd (fplus f)    -- catch coord case with plus
    isCoord _ = False

    atbh :: Int -> [String] -> [WS] -> [SO]  -- collect additional comps with same hs extracted
    atbh _ _ [] = []
    atbh i hs (pws:pwss) = let (hs', pso) = h i ((head.fst) pws) in
        if hs' == hs then pso:atbh i hs pwss else error "atbh error"
