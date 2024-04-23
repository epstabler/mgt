module MgL where  -- Multiset needed. E.g., start ghci with: stack ghci --package multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List
import qualified Data.List as List

import Mg

negWS :: WS -> Bool
negWS ws = fst (MultiSet.partition ((/= []).fst.snd) ws) /= (MultiSet.empty)

ell :: SO -> WS
ell (L (w,l)) = (MultiSet.singleton (L (w,l), l))
ell (S s) = case List.partition negWS (map ell (MultiSet.toList s)) of
  ([nws],pws:pwss) -> case List.partition ((/= []).fst.snd) (MultiSet.toList nws) of
      ([(nso,(f:ns,ps))],others) -> case List.filter ((== f).head.snd.snd) others of
          [(pso,plabel)] -> if (fst.maxx) (MultiSet.toList pws) /= pso  -- IM
                            then (error "ell: move-over-merge violation")
                            else d [nws]
          [] -> d (nws:pws:pwss) -- EM

