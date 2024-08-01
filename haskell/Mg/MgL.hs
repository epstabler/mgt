module MgL where  -- Multiset needed. E.g., start ghci with: stack ghci multiset
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.List (partition)
import Mg

-- label so with deterministic bottom-up transduction, applying d at every internal node
ell :: SO -> WS
ell so = case so of { (L lex) -> ([L lex], [snd lex]) ; (S s) -> d (map ell (MultiSet.toList s)) }
