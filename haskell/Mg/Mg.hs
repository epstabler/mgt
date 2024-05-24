-- https://github.com/epstabler/mgt/tree/main/haskell/Mg/Mg.hs
module Mg where     -- Multiset needed. E.g., start ghci with: stack ghci --package multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List (partition)

data F = C | D | N | V | A | P | Wh | Pred | Predx | T | K | Vx | Scr | Modal | Have | Be | Been |
         Ving | Ven | Lf | Rt | B | Visa | Vgelem deriving (Show, Eq, Ord)
data Ft = One F | Plus F deriving (Show, Eq, Ord)
type Label = ([Ft], [Ft])
type Lex = ([String], Label)
data PhTree = Pl Lex | Ps [PhTree] deriving (Show, Eq, Ord)
data SO = L Lex | S (MultiSet SO) | O PhTree deriving (Show, Eq, Ord)
type WS = ([SO],[Label])

-- basics: pair cons, pair concatenation, pair partition, feature parser
(a,b) @@ (x,y) = (a:x, b:y)
(x,y) +++ (z,w) = (x ++ z, y ++ w)
ppartition _ ([],[]) = (([],[]),([],[]))
ppartition p (f:fs, s:ss) = let (ps,nonps) = ppartition p (fs,ss) in
  if p f s then ((f,s) @@ ps, nonps) else (ps, (f,s) @@ nonps)
fplus ft = case ft of (One f) -> (f, False); (Plus f) -> (f, True)

-- merge
mrg :: [SO] -> SO
mrg sos = S (MultiSet.fromList sos)

-- already matched features can be `forgotten'
ck :: [Label] -> [Label]
ck ((_:nns,nps):([],_:pps):more) = [(nns,nps), ([],pps) ] ++ map (const ([],[])) more

-- constituents `forgotten' from workspace when 'inert', i.e. all features 'forgotten'
t :: [SO] -> [Label] -> WS
t (_:sos) (([],[]):labels) = t sos labels
t (so:sos) (label:labels) = (so,label) @@ t sos labels
t [] [] = ([], [])

match:: [WS] -> (WS,WS)
match wss = 
  let ([(so:sos,label:labels)],poswss) = partition ((/= []).fst.head.snd) wss in  -- partition neg WSs
  let (f,plus) = (fplus.head.fst) label in
    case (ppartition (\x y -> ((== One f).head.snd) y) (sos,labels), poswss) of  -- partition matches
      ((([so'],[label']), imOthers), []) -> ( ([so,so'],[label,label']), imOthers )           -- IM
      ((([],[]), imOthers), ws:wss') -> case ppartition (\x y -> ((== One f).head.snd) y) ws of -- EM
        (([so'],[label']), emOthers) -> 
          if plus && emOthers == imOthers
          then ( ([so,so'],[label,label']) +++ atb label' emOthers wss', imOthers)
          else if null wss' then ( ([so,so'],[label,label']), imOthers +++ emOthers) else error "match"
  where
    atb :: Label -> WS -> [WS] -> WS  -- collect additional comps with same label and movers
    atb _ _ [] = ([],[])
    atb l movers (ws:wss) =
      let (([so'],[label']), others) = ppartition (\x y -> y == l) ws in -- partition matches
        if others == movers then ((so',label') @@ (atb l movers wss)) else error "match: ATB error"

-- if labels of WS satisfy shortest move constraint, return WS; else crash -- 'too much to remember!'
smc :: WS -> WS
smc (sos,labels) = if smc' [] labels then (sos,labels) else error "smc violation" where
  smc' _ [] = True
  smc' sofar (([],p:ps):labels) = let (f,_) = fplus p in if f `elem` sofar then False else smc' (f:sofar) labels
  smc' sofar (_:labels) = smc' sofar labels

-- the derivational step: binary merge and label
d :: [WS] -> WS
d wss = let ((sos,labels),others) = match wss in
  smc (t (mrg sos:tail sos) (ck labels) +++ others)
