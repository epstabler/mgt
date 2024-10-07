module MgBinO where
import Data.MultiSet (MultiSet, toList) -- Multiset needed. E.g. use: start ghci -package multiset
import MgBin (SO(S,L,O), PhTree(Pl,Ps), WS, ell, wssNeg, wsPosMatch)

o_svo :: SO -> SO
o_svo (O t) = O t
o_svo (L (w,l)) = O (Pl (w,l))
o_svo so = let (nso, pso, _, posfs) = o so in case (o_svo nso, o_svo pso, posfs) of
               (O (Pl i),O t,_:_:_) -> O (Ps [Pl i, silent t]) -- first merged, moving
               (O (Pl i),O t,_) -> O (Ps [Pl i,t])             -- first merged
               (O t,O t',_:_:_) -> O (Ps [silent t',t])        -- non-first merged, moving
               (O t,O t',_) -> O (Ps [t',t])                   -- non-first merged

o_sov :: SO -> SO
o_sov (O t) = O t
o_sov (L (w,l)) = O (Pl (w,l))
o_sov so = let (nso, pso, _, posfs) = o so in case (o_sov nso, o_sov pso, posfs) of
               (O t,O t',_:_:_) -> O (Ps [silent t',t])
               (O t,O t',_) -> O (Ps [t',t])

-- reveal what order depends on: (head SO, comp SO, pos head features, pos comp features)
o :: SO -> (SO,SO,[String],[String])
o (S s) = let ([nws],[pws]) = wssNeg (map ell (toList s)) in
  let (so:sos,(f:_,_):labels) = nws in case wsPosMatch f (sos,labels) of
    (([so'],[label']), _) -> ( (head.fst) nws, (head.fst) pws, (snd.head.snd) nws, snd label' )
    _ ->                     ( (head.fst) nws, (head.fst) pws, (snd.head.snd) nws, (snd.head.snd) pws )

-- tentatively, for readability: instead of deleting moved PhTree, parenthesize its yield
silent :: PhTree -> PhTree
silent (Pl (ws,label)) = Pl (map (\w -> if head w == '(' && last w == ')' then w else "(" ++ w ++ ")") ws, label)
silent (Ps phs) = Ps (map silent phs)
