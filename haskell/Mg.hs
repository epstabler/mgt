module Mg where     -- Multiset needed. E.g., start ghci with: stack ghci --package multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List
import qualified Data.List as List

data F = C | D | N | V | A | P | Wh | Pred | Predx | T | K | Vx | Scr |
         Modal | Have | Be | Been | Ving | Ven deriving (Show, Eq, Ord)
data Ft = One F | Plus F deriving (Show, Eq, Ord) -- build features from base F
type Label = ([Ft], [Ft])
type Lex = ([String], Label)
data PhTree = Pl Lex | Ps [PhTree] deriving (Show, Eq, Ord)
data SO = L Lex | S (MultiSet (SO)) | O PhTree deriving (Show, Eq, Ord)
type LSO = (SO, Label)
type WS = MultiSet (LSO)

mrg :: [SO] -> SO
mrg sos = S (MultiSet.fromList sos)

ck :: [Label] -> [Label] -- already matched features can be `forgotten'
ck ((_:nns,nps):([],_:pps):more) = [(nns,nps), ([],pps) ] ++ (map (\label -> ([],[])) more)

t :: [LSO] -> [LSO]      -- constituents with no remembered features can be `forgotten'
t = filter (\lso -> snd lso /= ([],[]))

soSize :: SO -> Int
soSize (S so) = foldr (\x y -> (soSize x) + y) 1 (MultiSet.toList so)
soSize (O (Ps ts)) = foldr (\x y -> (soSize (O x)) + y) 1 ts
soSize _ = 1

maxx :: [LSO] -> LSO
maxx lsos = foldr1 (\x y ->if ((soSize.fst) x) >= ((soSize.fst) y) then x else y) lsos

match:: [WS] -> ([LSO],[LSO])
match (ws0:wss) = case List.partition ((/= []).fst.snd) (MultiSet.toList ws0) of
    ([h],others) -> let (f,plus) = (fplus.head.fst.snd) h in
      case (List.partition ((== (One f)).head.snd.snd) others,wss) of
        (([c],others'),[]) -> ([h,c],others')                                        -- IM
        (([],_),ws1:wss) -> let lsos = (MultiSet.toList ws1) in let c = maxx lsos in -- EM
          if ((/= One f).head.snd.snd) c
          then error "match complement feature clash"
          else let others' = List.filter (/= c) lsos in
            if plus && others' == others
            then (h:c:(atb (One f) others wss), others)
            else if wss == [] then ([h,c], others ++ others') else (error ("match: too many wss"))
  where
    fplus :: Ft -> (F, Bool)             -- parse the one/plus features
    fplus ft = case ft of (One f) -> (f, False); (Plus f) -> (f, True)

    atb :: Ft -> [LSO] -> [WS] -> [LSO]  -- collect comps with first feature f, and others
    atb _ _ [] = []
    atb ft movers (ws:wss) = case List.partition ((== ft).head.snd.snd) (MultiSet.toList ws) of
        ([c'],others) -> if others == movers then c':(atb ft movers wss) else (error "match: ATB error")

smc :: [LSO] -> WS
smc lsos = if smc' [] lsos then MultiSet.fromList lsos else error "smc violation"
  where
    smc' _ [] = True
    smc' sofar ((s,([],p:ps)):lsos) = if elem p sofar then False else smc' (p:sofar) lsos
    smc' sofar (_:lsos) = smc' sofar lsos

-- the derivational step: unbounded merge and label
d :: [WS] -> WS
d wss = let (matched,movers) = match wss in
        let (sos,labels) = unzip matched in
           smc (t (zip (mrg sos:tail sos) (ck labels) ++ movers))
