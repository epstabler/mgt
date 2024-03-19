module MgBinLinearization where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import qualified Data.List as List

import MgBin
import MgBinTransduction

ord_svo :: SO -> SO
ord_svo (O t) = (O t) -- NB! recurse only as deeply as necessary
ord_svo (L (w,l)) = (O (Pl (w,l)))
ord_svo so = let (nso, pso, _, posfs) = ord(so) in 
                case (ord_svo nso, ord_svo pso, posfs) of
                  (O (Pl i),O t,_:_:_) -> (O (Ps [(Pl i), silent t]))
                  (O (Pl i),O t,_) -> (O (Ps [(Pl i), t]))
                  (O t,O t',_:_:_) -> (O (Ps [silent t', t]))
                  (O t,O t',_) -> (O (Ps [t',t]))

ord_sov :: SO -> SO
ord_sov (O t) = (O t) -- NB! recurse only as deeply as necessary
ord_sov (L (w,l)) = (O (Pl (w,l)))
ord_sov so = let (nso, pso, _, posfs) = ord(so) in 
               case (ord_sov nso, ord_sov pso, posfs) of
                 (O t,O t',_:_:_) -> (O (Ps [silent t', t]))
                 (O t,O t',_) -> (O (Ps [t', t]))

-- map SO to what ordering usually depends on: (head, comp, head_features, comp_features)
ord :: SO -> (SO,SO,[Ft],[Ft])
ord (S s) = case List.partition negWS (map ell (Set.toList s)) of -- NB! inefficient
    ([nws],[pws]) -> case List.partition ((/= []).fst.snd) (Set.toList nws) of
       ([(nso,(f:ns,ps))],others) -> case List.filter ((== f).head.snd.snd) others of
          [(pso,([],posfs))] -> (nso, pso, ps, posfs)        -- for IM, else EM:
          [] -> let (pso,([],posfs)) = (maxx.(Set.toList)) pws in (nso, pso, ps, posfs)

silent :: PhTree -> PhTree
silent (Pl (ws,label)) = (Pl ((map (\w -> if head w == '(' && last w == ')'
                                          then w 
                                          else ("(" ++ w ++ ")") ) ws),label))
silent (Ps phs) = Ps (map silent phs)
