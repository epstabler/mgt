-- https://github.com/epstabler/mgt/tree/main/haskell/MgBin/MgBinL.hs
module MgBinL where  -- "L" for "Label". Partial function ell maps SO to its WS, if any
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (partition)
import MgBin

ell :: SO -> WS
ell (L lex) = ([L lex], [snd lex])
ell (S s) = let ([nws],[pws]) = partition ((/= []).fst.head.snd) (map ell (Set.toList s)) in       -- partition neg WS
  let (so:sos,(f:_,_):labels) = nws in case ppartition (\x y -> ((==f).head.snd) y) (sos,labels) of -- partition matches
    (([so'],_), _) -> if (head.fst) pws /= so'                       -- IM
                      then error "ell: merge-over-move violation"
                      else d [nws]
    _ -> d [nws,pws]                                                 -- EM
