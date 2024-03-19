module MgBinTransduction where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import qualified Data.List as List

import MgBin

negWS :: WS -> Bool
negWS ws = fst (Set.partition ((/= []).fst.snd) ws) /= (Set.empty)

ell :: SO -> WS
ell (L (w,l)) = (Set.singleton (L (w,l), l))
ell (S s) = case List.partition negWS (map ell (Set.toList s)) of
  ([nws],[pws]) -> case List.partition ((/= []).fst.snd) (Set.toList nws) of
      ([(nso,(f:ns,ps))],others) -> case List.filter ((== f).head.snd.snd) others of
          [(pso,plabel)] -> if (fst.maxx) (Set.toList pws) /= pso -- IM
                            then (error "ell: move-over-merge violation")
                            else d [nws]
          [] -> d [nws,pws] -- EM

