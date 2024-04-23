module MgBin where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import qualified Data.List as List

data Ft = C | D | N | V | A | P | Wh deriving (Show, Eq, Ord)
type Label = ([Ft], [Ft])
type Lex = ([String], Label)
data PhTree = Pl Lex | Ps [PhTree] deriving (Show, Eq, Ord)
data SO = L Lex | S (Set (SO)) | O PhTree deriving (Show, Eq, Ord)
type LSO = (SO, Label)
type WS = Set (LSO)

-- merge
mrg :: [SO] -> SO
mrg sos = S (Set.fromList sos)

-- already matched features can be `forgotten'
ck :: [Label] -> [Label]
ck [ (_:nns,nps), ([],_:pps) ] = [ (nns,nps), ([],pps) ]

-- constituents with no remembered features can be `forgotten'
t :: [LSO] -> [LSO]
t = filter (\lso -> snd lso /= ([],[]))

-- return number of nodes in SO (here we can disregard multidominance)
soSize :: SO -> Int
soSize (S so) = foldr (\x y -> (soSize x) + y) 1 (Set.toList so)
soSize (O (Ps ts)) = foldr (\x y -> (soSize (O x)) + y) 1 ts
soSize _ = 1

-- given LSOs, return largest, i.e. the head
maxx :: [LSO] -> LSO
maxx lsos = foldr1 (\x y ->if ((soSize.fst) x) >= ((soSize.fst) y) then x else y) lsos

-- given WS, return matching ([head,comp], [other LSOs])
match:: [WS] -> ([LSO],[LSO])
match (ws0:wss) = case List.partition ((/= []).fst.snd) (Set.toList ws0) of
    ([h],others) -> let f = (head.fst.snd) h in case (List.partition ((== f).head.snd.snd) others, wss) of
      (([c],others'),[]) -> ([h,c],others')                                            -- IM
      (([],_),[ws1]) -> case List.partition ((== f).head.snd.snd) (Set.toList ws1) of  -- EM
        ([c],others') -> ([h,c], others ++ others')

-- if LSOs satisfy shortest move constraint, return WS
smc :: [LSO] -> WS
smc lsos = if smc' [] lsos then Set.fromList lsos else error "smc violation"
  where
    smc' _ [] = True
    smc' sofar ((s,([],p:ps)):lsos) = if elem p sofar then False else smc' (p:sofar) lsos
    smc' sofar (_:lsos) = smc' sofar lsos

-- the derivational step: binary merge and label
d :: [WS] -> WS
d wss = let (matched,movers) = match wss in
        let (sos,labels) = unzip matched in
           smc (t (zip (mrg sos:tail sos) (ck labels) ++ movers))
