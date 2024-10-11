module MgBinO where
import Data.MultiSet (MultiSet, toList) -- Multiset needed. E.g. use: start ghci -package multiset
import MgBin (SO(S,L,O), PhTree(Pl,Ps), WS, ell, wssNeg, wsPosMatch)

o_svo :: SO -> SO
o_svo (L lex) = O (Pl lex)
o_svo (S s) = let ([(nso:nsos,nlabel:nlabels)],poswss) = wssNeg (map ell (toList s)) in
              let f = (head.fst) nlabel in case (wsPosMatch f (nsos,nlabels), poswss) of
               ((([pso],[plabel]), _), _) ->  formatPhTree (length (snd plabel)) nso pso -- IM
               ((([],[]), _), [ws]) -> case wsPosMatch f ws of
                 (([pso],[plabel]), _) -> formatPhTree (length (snd plabel)) nso pso -- EM
  where formatPhTree noOfPsoPosFeats nso pso = case (noOfPsoPosFeats, nso, o_svo nso, o_svo pso) of
            (1, L _, O pht, O pht') -> O (Ps [pht, pht']) -- first merge, nonmoving complement
            (1, S _, O pht, O pht') -> O (Ps [pht', pht]) -- nonfirst merge, nonmoving complement
            (_,   _, O pht, _) -> O pht                   -- moving complement
