module MgBinH where
import Data.MultiSet (MultiSet, fromList, toList) -- Multiset needed. E.g. use: ghci -package multiset
import MgBin (SO(S,L), ell, wssNeg, wsPosMatch )
import Data.Bifunctor (first)

hfeats :: [String] -> (Int, Bool, [String]) -- returns (noOfProbes, strong?, head without feats)
hfeats [] = (0, False, [])
hfeats (s:ss) = let (i,s') = deps s in if not (null s') && [last s'] == "*" then (i,True,init s':ss) else (i,False,s':ss)
  where deps s = case s of {('-':s') -> first (1+) (deps s') ; _ -> (0,s)}

heng :: SO -> SO -- h' (noOfHeadsNeededAbove,strong?,split?,headsFromAbove) = (break?,headsFromBelow,so)
heng so = case h' 0 False False [] so of { (_, [], so') -> so' } where
  h' :: Int -> Bool -> Bool -> [String] -> SO -> (Bool, [String], SO)
  h' 0 _ _ [] (L lex) = (False, [], L lex)
  h' 1 hiStrong sp hs (L (w,fs)) = let (i',isStrong,w') = hfeats w in let cat = (head.snd.fst) fs in
    if isStrong && not hiStrong then (cat == "V", [], L(w'++hs,fs)) else (cat == "V", w'++hs, L([],fs))
  h' i hiStrong sp hs (S s) = let ([nws],pws:pwss) = wssNeg (map ell (toList s)) in case (head.fst) nws of
      L (w,fs) -> let cat = (head.snd.fst) fs in let (i',strong,w') = hfeats w in
        let i'' = i' + max 0 (i-1) in case (i,i'') of
          (0,0) -> case h' 0 False False [] ((head.fst) pws) of                  -- no chain
            { (br, [], pso) -> (br, [], S (fromList [L (w, fs) , pso ])) }
          (0,1) -> let (br,hs',pso) = h' i'' strong False w' ((head.fst) pws) in -- chain begins
            (br, [], S (fromList [L (hs', fs), pso ]))
          (0,_) -> let (br,hs',pso) = h' i'' strong False [] ((head.fst) pws) in -- chain begins
            (br, [], S (fromList [L (w' ++ foldl (flip (:)) [] hs', fs), pso]))
          (1,0) -> let (br,hs',pso) = h' 0 False False [] ((head.fst) pws) in    -- chain ends
            if strong && not hiStrong
            then (cat == "V", [], S (fromList [L (w' ++ hs, fs), pso]))
            else (cat == "V", w' ++ hs, S (fromList [L ([], fs), pso]))
          (_,_) -> if sp && (head.snd.fst) fs == "v" && strong
                   then let (br, hs',pso) = h' i'' strong False w' ((head.fst) pws) in
                     if br
                     then (br, "DO":hs, S (fromList [L (hs' , fs), pso]))
                     else if strong && not hiStrong
                          then (br, [], S (fromList [L (hs' ++ hs , fs), pso]))
                          else (br, hs' ++ hs , S (fromList [L ([], fs), pso]))
                  else let (br, hs',pso) = h' i'' (max strong hiStrong) sp (w' ++ hs) ((head.fst) pws) in
                      if strong && not hiStrong
                      then (br, [], S (fromList [L (hs', fs), pso]))
                      else (br, hs', S (fromList [L ([], fs), pso]))
      nso -> if not sp && nonMovingPso nws pws
        then let (br,hs',nso') = h' i hiStrong True hs nso in let psos = map (head.fst) (pws:pwss) in
               (br, hs', S (fromList (nso' : psos)))
        else let (br,hs',nso') = h' i hiStrong sp hs nso in let psos = map (head.fst) (pws:pwss) in
               (br, hs', S (fromList (nso' : psos)))

nonMovingPso nws pws = let (nso:nsos,nlabel:nlabels) = nws in
  let f = (head.fst) nlabel in case wsPosMatch f (nsos,nlabels) of
    (([pso],[plabel]), _) -> length (snd plabel) <= 1
    (([],[]), _) -> case pws of { (_,plabel:_) -> length (snd plabel) <= 1 }
