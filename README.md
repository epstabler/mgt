# mmg
Modular minimalist grammar

See the draft paper Stabler24-mmg.pdf. The slides present an outline and code listing.

** preliminary implementations, in progress **

** The code in haskell/MgBin is the most stable **

## haskell/

An implementation of merge, *-merge, with labeling, head movement, linearization, and m-merger.

Haskell is a typed higher order language, good for carefully assessing the anatomy of these processes.
This code provides the clearest formulation of this grammar formalism.

## python/ 

Ported from the Haskell, a Python 3.x implementation of merge, *-merge, labeling, interfaces.

