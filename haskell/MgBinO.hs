module MgBinO where
import Data.MultiSet (MultiSet, toList) -- Multiset needed. E.g. use: start ghci -package multiset
import MgBin (SO(S,L,O), PhTree(Pl,Ps), WS, ell, wssNeg, wsPosMatch)

o_svo :: SO -> SO
o_svo (O t) = O t
o_svo (L (w,l)) = O (Pl (w,l))
o_svo so = let (nso, pso, _, posfs) = o so in case (o_svo nso, o_svo pso, posfs) of
               (O (Pl i),O t,_:_:_) -> O (Ps [Pl i, Pl ([],(([],[]), ("","")))])   -- t 1st merge, moving
               (O (Pl i),O t,_) -> O (Ps [Pl i,t])                                 -- t 1st merge
               (O t,O t',_:_:_) -> O (Ps [Pl ([],(([],[]), ("",""))), t])          -- t' non-1st merge, moving
               (O t,O t',_) -> O (Ps [t',t])                                       -- t' non-1st merge


o :: SO -> (SO,SO,[String],[String]) -- expose (head SO, comp SO, pos head features, pos comp features)
o (S s) = let ([nws],[pws]) = wssNeg (map ell (toList s)) in
  let (so:sos,(f:_,_):labels) = nws in case wsPosMatch f (sos,labels) of
    (([so'],[label']), _) -> ( (head.fst) nws, (head.fst) pws, (snd.head.snd) nws, snd label' )
    _ ->                     ( (head.fst) nws, (head.fst) pws, (snd.head.snd) nws, (snd.head.snd) pws )
