module MgLinearization where  -- Multiset needed. E.g., start ghci with: stack ghci --package multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List
import qualified Data.List as List

import Mg
import MgTransduction

ord_svo :: SO -> SO
ord_svo (O t) = (O t) -- NB! recurse only as deeply as necessary
ord_svo (L (w,l)) = (O (Pl (w,l)))
ord_svo so = let (nso, pso, _, posfs, pwss) = ord(so) in 
             let plsos = (map (maxx.(MultiSet.toList)) pwss) in
             let ts = map (\x -> case (ord_svo.fst) x of (O t) -> t) plsos in
                case (ord_svo nso, ord_svo pso, posfs) of
                  -- (O (Pl i),O t,_:_:_) -> (O (Ps ((Pl i):(map silent (t:ts)))))
                  (O (Pl i),O t,_:_:_) -> (O ((Pl i)))
                  (O (Pl i),O t,_) -> (O (Ps ((Pl i):t:ts)))
                  -- (O t,O t',_:_:_) -> (O (Ps (((silent t'):(map silent ts))++[t])))
                  (O t,O t',_:_:_) -> (O t)
                  (O t,O t',_) -> (O (Ps (t':(ts ++ [t]))))

ord_sov :: SO -> SO
ord_sov (O t) = (O t) -- NB! recurse only as deeply as necessary
ord_sov (L (w,l)) = (O (Pl (w,l)))
ord_sov so = let (nso, pso, _, posfs, pwss) = ord(so) in 
             let plsos = (map (maxx.(MultiSet.toList)) pwss) in
             let ts = map (\x -> case (ord_sov.fst) x of (O t) -> t) plsos in
               case (ord_sov nso, ord_sov pso, posfs) of
                 -- (O t,O t',_:_:_) -> (O (Ps ((silent t':(map silent ts))++[t])))
                 (O t,O t',_:_:_) -> (O t)
                 (O t,O t',_) -> (O (Ps ((t':ts)++[t])))

-- map SO to what ordering usually depends on: (head, comp, head_features, comp_features, otherCompWSs)
ord :: SO -> (SO,SO,[Ft],[Ft],[WS])
ord (S s) = case List.partition negWS (map ell (MultiSet.toList s)) of -- NB! inefficient
    ([nws],pws:pwss) -> case List.partition ((/= []).fst.snd) (MultiSet.toList nws) of
        ([(nso,(f:ns,ps))],others) -> case ((List.filter ((== f).head.snd.snd) others),pwss) of
            ([(pso,([],posfs))],[]) -> (nso, pso, ps, posfs, pwss)        -- for IM, else EM:
            ([],_) -> let (pso,([],posfs)) = (maxx.(MultiSet.toList)) pws in (nso, pso, ps, posfs, pwss)

-- silent :: PhTree -> PhTree
-- silent (Pl (ws,label)) = (Pl ((map (\w -> "(" ++ w ++ ")") ws),label))
-- silent (Ps phs) = Ps (map silent phs)
