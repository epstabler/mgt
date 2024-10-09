module MgBinH where
import Data.MultiSet (MultiSet, fromList, toList) -- Multiset needed. E.g. use: ghci -package multiset
import MgBin (SO(S,L), ell, wssNeg, wsPosMatch )

hfeats :: [String] -> (Int, Bool, [String])
hfeats [] = (0, False, [])
hfeats (s:ss) = let (i,s') = inc s in if length s' > 0
                                      then if last s' == '*' then (i,True,init s':ss) else (i,False,s':ss)
                                      else (i,False,s':ss)
  where inc [] = (0, [])
        inc s = if head s == '-' then let (i,s') = inc (tail s) in (1+i,s') else (0,s)

h :: SO -> SO
h so = case h' 0 False False [] so of { ([], so') -> so' } where
  -- h' i st sp hs so, with i = no. of heads needed above, st = strong?, sp = split?, hs = heads from above
  h' :: Int -> Bool -> Bool -> [String] -> SO -> ([String], SO)
  h' 0 _ _ [] (L lex) = ([], L lex)
  h' 1 hiStrong sp hs (L (w,fs)) = let (i',isStrong,w') = hfeats w in 
    if isStrong && not hiStrong then ([], L(w'++hs,fs)) else (w'++hs, L([],fs))
  h' i hiStrong sp hs (S s) =
    let ([nws],pws:pwss) = wssNeg (map ell (toList s)) in case (head.fst) nws of
      L (w,fs) -> let (i',strong,w') = hfeats w in let i'' = i' + max 0 (i-1) in case (i,i'') of
          (0,0) -> case h' 0 False False [] ((head.fst) pws) of               -- no head chain
            { ([], pso) -> ([], S (fromList (L (w, fs) : pso : []))) }
          (0,1) -> let (hs',pso) = h' i'' strong False w' ((head.fst) pws) in -- head chain begins
            ([], S (fromList (L (hs', fs) : pso : [])))
          (0,_) -> let (hs',pso) = h' i'' strong False [] ((head.fst) pws) in -- head chain begins
            ([], S (fromList (L (w' ++ (foldl (\acc x -> x : acc) [] hs'), fs) : pso : [])))
          (1,0) -> 
            if sp
            then error "h: broken chain" -- if sp = True at the bottom this chain (i.e. no v*) -> error
            else let (hs',pso) = h' 0 False False [] ((head.fst) pws) in    -- head chain ends
                 if strong && not hiStrong
                 then ([], S (fromList (L (w' ++ hs, fs) : pso : [])))
                 else (w' ++ hs, S (fromList (L ([], fs) : pso : [])))
          (_,_) -> -- if sp = True and lex category = v*: new chain starts; DO inserted at end of current chain
            if sp && (head.snd.fst) fs == "v" && strong
            then let (hs',pso) = h' i'' strong False w' ((head.fst) pws) in
              (["DO"] ++ hs, S (fromList (L (hs' , fs) : pso : [])))
            else
              let (hs',pso) = h' i'' (max strong hiStrong) sp (w' ++ hs) ((head.fst) pws) in
                if strong && not hiStrong
                then ([], S (fromList (L (hs', fs) : pso : [])))
                else (hs', S (fromList (L ([], fs) : pso : [])))
{-
          (_,_) -> let (hs',pso) = h' i'' (max strong hiStrong) sp (w' ++ hs) ((head.fst) pws) in -- mult
            if strong && not hiStrong
            then ([], S (fromList (L (foldl (\acc x -> x : acc) [] hs', fs) : pso : [])))
            else (foldl (\acc x -> x : acc) [] hs', S (fromList (L ([], fs) : pso : [])))
-}
      nso ->
        if not sp && nonMovingIntervener nws pws
        then let (hs',nso') = h' i hiStrong True hs nso in let psos = map (head.fst) (pws:pwss) in
               (hs', S (fromList (nso' : psos)))
        else let (hs',nso') = h' i hiStrong sp hs nso in let psos = map (head.fst) (pws:pwss) in
               (hs', S (fromList (nso' : psos)))
    where
      nonMovingIntervener nws pws = let (nso:nsos,nlabel:nlabels) = nws in
        let f = (head.fst) nlabel in case wsPosMatch f (nsos,nlabels) of
          (([pso],[plabel]), _) -> if (length (snd plabel)) > 1 then False else True
          (([],[]), _) -> case wsPosMatch f pws of
            (([pso],[plabel]), _) -> if (length (snd plabel)) > 1 then False else True


hjava :: SO -> SO
hjava so = case j' 0 False False [] so of { ([], so') -> so' } where
  -- j' i st sp hs so, with i = no. of heads needed above, st = strong?, sp = split?, hs = heads from above
  j' :: Int -> Bool -> Bool -> [String] -> SO -> ([String], SO)
  j' 0 _ _ [] (L lex) = ([], L lex)
  j' 1 hiStrong sp hs (L (w,fs)) = let (i',isStrong,w') = hfeats w in 
    if isStrong && not hiStrong then ([], L(w'++hs,fs)) else (w'++hs, L([],fs))
  j' i hiStrong sp hs (S s) =
    let ([nws],pws:pwss) = wssNeg (map ell (toList s)) in case (head.fst) nws of
      L (w,fs) -> let (i',strong,w') = hfeats w in let i'' = i' + max 0 (i-1) in case (i,i'') of
          (0,0) -> case j' 0 False False [] ((head.fst) pws) of               -- no head chain
            { ([], pso) -> ([], S (fromList (L (w, fs) : pso : []))) }
          (0,1) -> let (hs',pso) = j' i'' strong False w' ((head.fst) pws) in -- head chain begins
            ([], S (fromList (L (hs', fs) : pso : [])))
          (0,_) -> let (hs',pso) = j' i'' strong False [] ((head.fst) pws) in -- multiple head chain begins
            ([], S (fromList (L (w' ++ (foldl (\acc x -> x : acc) [] hs'), fs) : pso : [])))
          (1,0) -> let (hs',pso) = j' 0 False False [] ((head.fst) pws) in    -- head chain ends
             if strong && not hiStrong
             then ([], S (fromList (L (w' ++ hs, fs) : pso : [])))
             else (w' ++ hs, S (fromList (L ([], fs) : pso : [])))
          (_,_) -> let (hs',pso) = j' i'' (max strong hiStrong) sp (w' ++ hs) ((head.fst) pws) in
                if strong && not hiStrong
                then ([], S (fromList (L (hs', fs) : pso : [])))
                else (hs', S (fromList (L ([], fs) : pso : [])))
      nso -> let (hs',nso') = j' i hiStrong sp hs nso in let psos = map (head.fst) (pws:pwss) in
        (hs', S (fromList (nso' : psos)))
