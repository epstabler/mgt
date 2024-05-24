-- https://github.com/epstabler/mgt/tree/main/haskell/MgBin/MgBinO.hs
module MgBinO where -- "O" for "Ordered". This is the linearization step.
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (partition)
import MgBin
import MgBinL
import MgBinH

o_svo :: SO -> SO
o_svo (O t) = O t -- NB! recurse only as deeply as necessary
o_svo (L (w,l)) = O (Pl (w,l))
o_svo so = let (nso, pso, _, posfs) = o so in 
             case (o_svo nso, o_svo pso, posfs) of
               (O (Pl i),O t,_:_:_) -> O (Ps [Pl i, silent t])
               (O (Pl i),O t,_) -> O (Ps [Pl i, t])
               (O t,O t',_:_:_) -> O (Ps [silent t', t])
               (O t,O t',_) -> O (Ps [t',t])

o_sov :: SO -> SO
o_sov (O t) = O t -- NB! recurse only as deeply as necessary
o_sov (L (w,l)) = O (Pl (w,l))
o_sov so = let (nso, pso, _, posfs) = o so in 
             case (o_sov nso, o_sov pso, posfs) of
               (O t,O t',_:_:_) -> O (Ps [silent t', t])
               (O t,O t',_) -> O (Ps [t', t])

-- map SO to what ordering usually depends on: (head SO, comp SO, pos head features, pos comp features)
o :: SO -> (SO,SO,[Ft],[Ft])
o (S s) = let ([nws],[pws]) = partition ((/= []).fst.head.snd) (map ell (Set.toList s)) in -- partition neg WS
  -- NB: to get pos features of IM complement, we need to find them in (ell nws); otherwise, they're in pws
  let (so:sos,(f:_,_):labels) = nws in case ppartition (\x y -> ((==f).head.snd) y) (sos,labels) of -- partition matches
    (([so'],[label']), _) -> ( (head.fst) nws, (head.fst) pws, (snd.head.snd) nws, snd label' )
    _ ->                     ( (head.fst) nws, (head.fst) pws, (snd.head.snd) nws, (snd.head.snd) pws )

silent :: PhTree -> PhTree
silent (Pl (ws,label)) = Pl (map (\w -> if head w == '(' && last w == ')'
                                        then w 
                                        else "(" ++ w ++ ")") ws, label)
silent (Ps phs) = Ps (map silent phs)
