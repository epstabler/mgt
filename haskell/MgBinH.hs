module MgBinH where
import Data.MultiSet (MultiSet, fromList, toList) -- Multiset needed. E.g. use: ghci -package multiset
import MgBin (SO(S,L), ell, wssNeg, wsPosMatch )

hfeats :: [String] -> (Int, Bool, [String])
hfeats [] = (0, False, [])
hfeats (s:ss) = let (i,s') = inc s in if length s' > 0
                                      then if [last s'] == "*" then (i,True,init s':ss) else (i,False,s':ss)
                                      else (i,False,s':ss)
  where inc [] = (0, [])
        inc s = if head s == '-' then let (i,s') = inc (tail s) in (1+i,s') else (0,s)

h :: SO -> SO
h so = case h' 0 False False [] so of { ([], so') -> so' } where
  -- h' i st sp hs so, with i = no. of heads needed above, st = strong?, sp = split?, hs = head complex from above
  h' :: Int -> Bool -> Bool -> [String] -> SO -> ([String], SO)
  h' 0 _ _ [] (L lex) = ([], L lex)
  h' 1 hiStrong sp hs (L (w,fs)) = let (i',isStrong,w') = hfeats w in 
    if isStrong && not hiStrong then ([], L(w'++hs,fs)) else (w'++hs, L([],fs))
  h' i hiStrong sp hs (S s) =
    let ([nws],pws:pwss) = wssNeg (map ell (toList s)) in case (head.fst) nws of
      L (w,fs) -> let (i',strong,w') = hfeats w in let i'' = i' + max 0 (i-1) in case (i,i'') of
          (0,0) -> case h' 0 False False [] ((head.fst) pws) of               -- no head chain
            { ([], pso) -> ([], S (fromList (L (w, fs) : pso : []))) }
          (0,_) -> let (hs',pso) = h' i'' strong False w' ((head.fst) pws) in -- head chain begins
            ([], S (fromList (L (hs', fs) : pso : [])))
          (1,0) -> let (hs',pso) = h' 0 False False [] ((head.fst) pws) in    -- head chain ends
            if strong && not hiStrong
            then ([], S (fromList (L (w' ++ hs, fs) : pso : [])))
            else (w' ++ hs, S (fromList (L ([], fs) : pso : [])))
          (1,1) -> case ((snd.head.snd) pws) of  -- continuing chain: if nonmoving intervener, sp := True
              (_:_:_) -> let (hs',pso) = h' i'' (max strong hiStrong) sp (w' ++ hs) ((head.fst) pws) in
                      if strong && not hiStrong 
                      then ([], S (fromList (L (hs', fs) : pso : [])))
                      else (hs', S (fromList (L ([], fs) : pso : [])))
              _ -> let (hs',pso) = h' i'' (max strong hiStrong) True (w' ++ hs) ((head.fst) pws) in
                     if strong && not hiStrong
                     then ([], S (fromList (L (hs', fs) : pso : [])))
                     else (hs', S (fromList (L ([], fs) : pso : [])))
          (_,_) -> let (hs',pso) = h' i'' (max strong hiStrong) sp (w' ++ hs) ((head.fst) pws) in -- mult
            if strong && not hiStrong
            then ([], S (fromList (L (foldl (\acc x -> x : acc) [] hs', fs) : pso : [])))
            else (foldl (\acc x -> x : acc) [] hs', S (fromList (L ([], fs) : pso : [])))
      nso -> let (hs',nso') = h' i hiStrong sp hs nso in let psos = map (head.fst) (pws:pwss) in
          (hs', S (fromList (nso' : psos)))
