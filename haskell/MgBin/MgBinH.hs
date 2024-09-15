module MgBinH where
import Data.MultiSet (MultiSet, toList, fromList) -- Multiset needed. E.g., start ghci with: stack ghci multiset
import Data.List (partition)
import MgBin

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
          let i' = inc w + max 0 (i-1) in
          let (hs,pso) = h i' ((head.fst) pws) in case i of
            0 -> ([], S (fromList (L (hs ++ w, fs) : pso : [])))
            1 -> (hs ++ w, S (fromList (L ([], fs) : pso : [])))
            _ -> (w ++ hs, S (fromList (L ([], fs) : pso : [])))
      nso ->
          let (hs,nso') = h i nso in
          let psos = map (head.fst) (pws:pwss) in
            (hs, S (fromList (nso' : psos)))
