-- https://github.com/epstabler/mgt/tree/main/haskell/MgBin/MgBin.hs
module MgBin where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (partition)

data Ft = C | D | N | V | A | P | Wh | B | Lf | Rt | T | K | Vx |
          Modal | Have | Be | Been | Ving | Ven deriving (Show, Eq, Ord)
type Label = ([Ft], [Ft])
type Lex = ([String], Label)
data PhTree = Pl Lex | Ps [PhTree] deriving (Show, Eq, Ord)
data SO = L Lex | S (Set SO) | O PhTree deriving (Show, Eq, Ord)
type WS = ([SO],[Label])

-- basics: pair cons, pair concatenation, pair partition
(@@) :: (a1, a2) -> ([a1], [a2]) -> ([a1], [a2])
(a,b) @@ (x,y) = (a:x, b:y)

(+++) :: ([a1],[a2]) -> ([a1],[a2]) -> ([a1],[a2])
(x,y) +++ (z,w) = (x ++ z, y ++ w)

ppartition :: (a1 -> a2 -> Bool) -> ([a1],[a2]) -> (([a1],[a2]),([a1],[a2]))
ppartition _ ([],[]) = (([],[]),([],[]))
ppartition p (f:fs, s:ss) = let (ps,nonps) = ppartition p (fs, ss) in 
  if p f s then ((f,s) @@ ps, nonps) else (ps, (f,s) @@ nonps)

-- merge
mrg :: [SO] -> SO
mrg sos = S (Set.fromList sos)

-- already matched features can be `forgotten'
ck :: [Label] -> [Label]
ck [ (_:nns,nps), ([],_:pps) ] = [ (nns,nps), ([],pps) ]

-- constituents `forgotten' from workspace when 'inert', i.e. all features 'forgotten'
t :: [SO] -> [Label] -> WS
t (_:sos) (([],[]):labels) = t sos labels
t (so:sos) (label:labels) = (so,label) @@ t sos labels
t [] [] = ([], [])

-- given WSs, exactly 1 of which has a neg label, return (matching elements, others)
match:: [WS] -> (WS,WS)
match wss =
  let ([(so:sos,label:labels)],poswss) = partition ((/= []).fst.head.snd) wss in  -- partition neg WSs
  let f = (head.fst) label in
    case (ppartition (\x y -> ((== f).head.snd) y) (sos,labels), poswss) of -- partition matches
      ((([so'],[label']), others), []) -> ( ([so,so'],[label,label']), others )        -- EM
      ((([],[]), others), [ws]) -> case ppartition (\x y -> ((== f).head.snd) y) ws of -- IM
        (([so'],[label']), others') ->  ( ([so,so'],[label,label']), others +++ others')

-- if WS satisfies shortest move constraint, return it; else crash
smc :: WS -> WS
smc (sos,labels) = if smc' [] labels then (sos,labels) else error "smc violation" where
  smc' _ [] = True
  smc' sofar (([],p:ps):labels) = if p `elem` sofar then False else smc' (p:sofar) labels
  smc' sofar (_:labels) = smc' sofar labels

-- the derivational step: binary merge and label
d :: [WS] -> WS
d wss = let ((sos,labels),others) = match wss in
  smc (t (mrg sos:tail sos) (ck labels) +++ others)
