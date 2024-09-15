module MgO where
import Data.MultiSet (MultiSet, fromList, toList) -- Multiset needed. E.g., start ghci with: stack ghci multiset
import Data.List (partition)
import Mg
import MgH

o_svo :: SO -> SO
o_svo (O t) = O t -- NB! recurse only as deeply as necessary
o_svo (L (w,l)) = O (Pl (w,l))
o_svo so = let (nso, pso, _, posfs, pwss) = o so in 
           let psos = map (head.fst) pwss in
           let ts = map (\x -> case o_svo x of (O t) -> t) psos in
             case (o_svo nso, o_svo pso, posfs) of
               (O (Pl i),O t,_:_:_) -> O (Ps [Pl i, silent t]) -- first merged, moving
               (O (Pl i),O t,_) -> O (Ps (Pl i:t:ts))          -- first merged
               (O t,O t',_:_:_) -> O (Ps [silent t', t])       -- non-first merged, moving
               (O t,O t',_) -> O (Ps (t':(ts ++ [t])))         -- non-first merged

o_sov :: SO -> SO
o_sov (O t) = O t -- NB! recurse only as deeply as necessary
o_sov (L (w,l)) = O (Pl (w,l))
o_sov so = let (nso, pso, _, posfs, pwss) = o so in 
           let psos = map (head.fst) pwss in
           let ts = map (\x -> case o_svo x of (O t) -> t) psos in
             case (o_sov nso, o_sov pso, posfs) of
               (O t,O t',_:_:_) -> O (Ps [silent t', t])
               (O t,O t',_) -> O (Ps ((t':ts)++[t]))

-- reveal what order depends on: (head SO, comp SO, pos head features, pos comp features, addtl pos WSs)
o :: SO -> (SO,SO,[String],[String],[WS])
o (S s) = let ([nws],pws:pwss) = wssNeg (map ell (toList s)) in
  let (so:sos,(f:_,_):labels) = nws in case wsPosMatch f (sos,labels) of
    (([so'],[label']), _) -> ( (head.fst) nws, (head.fst) pws, (snd.head.snd) nws, snd label' , pwss )
    _ ->                     ( (head.fst) nws, (head.fst) pws, (snd.head.snd) nws, (snd.head.snd) pws, pwss )

silent :: PhTree -> PhTree
silent (Pl (ws,label)) = Pl (map (\w -> if head w == '(' && last w == ')'
                                        then w 
                                        else "(" ++ w ++ ")") ws, label)
silent (Ps phs) = Ps (map silent phs)
