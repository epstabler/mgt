-- https://github.com/epstabler/mgt/tree/main/haskell/Mg/MgL.hs
module MgL where  -- Multiset needed. E.g., start ghci with: stack ghci --package multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List (partition)
import Mg

ell :: SO -> WS
ell (L lex) = ([L lex], [snd lex])
ell (S s) = let ([nws],pws:pwss) = partition ((/= []).fst.head.snd) (map ell (MultiSet.toList s)) in -- partition neg WS
  let (so:sos,(f:_,_):labels) = nws in case ppartition (\x y -> ((==f).head.snd) y) (sos,labels) of -- partition matches
    (([so'],_), _) -> if (head.fst) pws /= so' || not (null pwss)    -- IM
                      then error "ell: merge-over-move violation"
                      else d [nws]
    _ -> d (nws:pws:pwss)                                            -- EM
