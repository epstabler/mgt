module MgO where  -- Multiset needed. E.g., start ghci with: stack ghci --package multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List
import qualified Data.List as List

import Mg
import MgL

o_svo :: SO -> SO
o_svo (O t) = O t -- NB! recurse only as deeply as necessary
o_svo (L (w,l)) = O (Pl (w,l))
o_svo so = let (nso, pso, _, posfs, pwss) = o so in
           let plsos = map (maxx.MultiSet.toList) pwss in
           let ts = map (\x -> case (o_svo.fst) x of (O t) -> t) plsos in
              case (o_svo nso, o_svo pso, posfs) of
                (O (Pl i),O t,_:_:_) -> O (Pl i)
                (O (Pl i),O t,_) -> O (Ps (Pl i:t:ts))
                (O t,O t',_:_:_) -> O t
                (O t,O t',_) -> O (Ps (t':(ts ++ [t])))

o_sov :: SO -> SO
o_sov (O t) = O t -- NB! recurse only as deeply as necessary
o_sov (L (w,l)) = O (Pl (w,l))
o_sov so = let (nso, pso, _, posfs, pwss) = o so in 
           let plsos = map (maxx.MultiSet.toList) pwss in
           let ts = map (\x -> case (o_sov.fst) x of (O t) -> t) plsos in
             case (o_sov nso, o_sov pso, posfs) of
               (O t,O t',_:_:_) -> O t
               (O t,O t',_) -> O (Ps ((t':ts)++[t]))

-- map SO to what ordering usually depends on: (head, comp, head_features, comp_features, otherCompWSs)
o :: SO -> (SO,SO,[Ft],[Ft],[WS])
o (S s) = case List.partition negWS (map ell (MultiSet.toList s)) of -- NB! inefficient
  ([nws],pws:pwss) -> case List.partition ((/= []).fst.snd) (MultiSet.toList nws) of
      ([(nso,(f:ns,ps))],others) -> case (List.filter ((== f).head.snd.snd) others,pwss) of
          ([(pso,([],posfs))],[]) -> (nso, pso, ps, posfs, pwss)           -- IM
          ([],_) -> let (pso,([],posfs)) = (maxx.MultiSet.toList) pws in 
            (nso, pso, ps, posfs, pwss)                                    -- EM
