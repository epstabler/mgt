# nmg
MG decomposed and reformulated with interfaces, inspired in part by 3 papers by Marcolli&al 2024:
["Mathematical structure of syntactic merge"](https://arxiv.org/abs/2305.18278),
["Old and new minimalism"](https://arxiv.org/abs/2306.10270),
["Syntax-semantics interface: An algebraic model"](https://arxiv.org/abs/2311.06189).

A paper describing the project is forthcoming.

** preliminary implementations, in progress **

## haskell/

An implementation of merge, *-merge, with labeling, head movement, linearization, and m-merger.

Haskell is a typed and higher order language, good for carefully assessing the anatomy of these processes.
This code provides the clearest formulation of this grammar formalism.

## python/ 

Ported from the Haskell, a Python 3.x implementation of merge, *-merge, labeling, interfaces.
