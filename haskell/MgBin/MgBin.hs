module MgBin where
import Data.MultiSet (MultiSet, toList, fromList)     -- Multiset needed. E.g., use: ghci -package multiset
import Data.List (partition)
import Data.Bifunctor (Bifunctor, bimap, first, second)

type Label = ([String], [String])
type Lex = ([String], Label)
data SO = L Lex | S (MultiSet SO) | O PhTree deriving (Show, Eq, Ord)
type WS = ([SO],[Label])
data PhTree = Pl Lex | Ps [PhTree] deriving (Show, Eq, Ord)

-- merge a sequence of SOs into one SO, one multiset
mrg :: [SO] -> SO
mrg sos = S (fromList sos)

-- append two pairs of lists, coordinate-wise
(+++) :: Bifunctor bf => ([a1], [a2]) -> bf [a1] [a2] -> bf [a1] [a2]
(xs,ys) +++ pairOfLists = bimap (xs++) (ys++) pairOfLists

-- partition a pair of lists (xs,ys) according to whether each (x_i,y_i) has property p
ppartition :: (a1 -> a2 -> Bool) -> ([a1], [a2]) -> (([a1], [a2]), ([a1], [a2]))
ppartition _ ([],[]) = (([],[]),([],[]))
ppartition p (x:xs, y:ys) = let (ps,nonps) = ppartition p (xs,ys) in
  if p x y then (bimap (x:) (y:) ps, nonps) else (ps, bimap (x:) (y:) nonps)

-- partition sequence of WSs to separate WS elements that have a negative element
wssNeg :: [WS] -> ([WS],[WS])
wssNeg = partition ((/= []).fst.head.snd)

-- partition WS elements to separate WS elements with labels beginning with positive feature f
wsPosMatch :: String -> WS -> (WS,WS)
wsPosMatch f = ppartition (\_ y -> ((== f).head.snd) y)

-- check/delete already matched features of head and complement
ck :: [Label] -> [Label]
ck [h,c] = [ first tail h, second tail c ]

-- delete inert, trival elements of a workspace
t :: WS -> WS
t = snd.ppartition (\_ y -> y == ([],[]))

-- partition list of workspaces into WS with neg f and matching pos f's, and WS of non-matching elements
match:: [WS] -> (WS,WS)
match wss = let ([(so:sos,label:labels)],poswss) = wssNeg wss in
  let f = (head.fst) label in case (wsPosMatch f (sos,labels), poswss) of
      ((([so'],[label']), imOthers), [(so'':_,_)]) ->         -- IM
        if so'' == so' then ( ([so,so'],[label,label']), imOthers ) else error "merge-over-move"
      ((([],[]), imOthers), [ws]) -> case wsPosMatch f ws of  -- EM
        (([so'],[label']), emOthers) -> ( ([so,so'],[label,label']), imOthers +++ emOthers )

-- return workspace if it respects smc, else error
smc :: WS -> WS
smc (sos,labels) = if smc' [] labels then (sos,labels) else error "smc violation" where
    smc' _ [] = True
    smc' sofar (([],f:_):labels) = (f `notElem` sofar) && smc' (f:sofar) labels
    smc' sofar (_:labels) = smc' sofar labels

-- derivational step, derives WS with a new merged SO and its new label
d :: [WS] -> WS
d wss = let ((sos,labels),others) = match wss in smc (t (mrg sos:tail sos, ck labels) +++ others)

-- extend d (partially) through the domain of merge
ell :: SO -> WS
ell (L lex) = ([L lex], [snd lex])
ell (S s) = d (map ell (toList s))
